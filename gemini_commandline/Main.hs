{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import System.Environment (getArgs, getEnv)
import qualified Data.Aeson as Aeson
import Data.Aeson (FromJSON, ToJSON, eitherDecode)
import GHC.Generics (Generic)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Client (newManager, httpLbs, parseRequest, Manager, Request(..), RequestBody(..), Response(..), responseStatus)
import Network.HTTP.Types.Status (statusCode)
-- Replace qualified import with explicit import list:
import Data.Text (Text, pack, unpack, splitOn, strip, null)
import Data.Text.Encoding (encodeUtf8)
import Control.Exception (SomeException, handle)

-- --- Request Data Types ---

data RequestPart = RequestPart
  { reqText :: Text  -- Using reqText to avoid name clash with Response Part's text
  } deriving (Show, Generic)

instance ToJSON RequestPart where
  toJSON (RequestPart t) = Aeson.object ["text" Aeson..= t]

data RequestContent = RequestContent
  { reqParts :: [RequestPart] -- Using reqParts to avoid name clash
  } deriving (Show, Generic)

instance ToJSON RequestContent where
  toJSON (RequestContent p) = Aeson.object ["parts" Aeson..= p]

data GenerationConfig = GenerationConfig
  { temperature     :: Double
  , maxOutputTokens :: Int
  -- Add other config fields as needed (e.g., topP, topK)
  } deriving (Show, Generic, ToJSON)

data GeminiApiRequest = GeminiApiRequest
  { contents         :: [RequestContent]
  , generationConfig :: GenerationConfig
  } deriving (Show, Generic, ToJSON)


-- --- Response Data Types (mostly unchanged, renamed for clarity) ---

data ResponsePart = ResponsePart
  { text :: String
  } deriving (Show, Generic, FromJSON)

data ResponseContent = ResponseContent
  { parts :: [ResponsePart]
  } deriving (Show, Generic, FromJSON)

data Candidate = Candidate
  { content :: ResponseContent
  } deriving (Show, Generic, FromJSON)

-- Assuming promptFeedback might be present at the top level of the response
-- alongside candidates, adjust if it's nested differently.
data SafetyRating = SafetyRating
  { category    :: String
  , probability :: String
  } deriving (Show, Generic, FromJSON)

data PromptFeedback = PromptFeedback
  { blockReason   :: Maybe String
  , safetyRatings :: Maybe [SafetyRating]
  } deriving (Show, Generic, FromJSON)

data GeminiApiResponse = GeminiApiResponse
  { candidates     :: [Candidate]
  , promptFeedback :: Maybe PromptFeedback -- Added optional promptFeedback
  } deriving (Show, Generic, FromJSON)

-- --- Completion Function ---

-- | Sends a prompt to the Gemini API and returns the completion text or an error.
completion :: String             -- ^ Google API Key
           -> Manager            -- ^ HTTP Manager
           -> String             -- ^ The user's prompt text
           -> IO (Either String String) -- ^ Left error message or Right completion text
completion apiKey manager promptText = do
  initialRequest <- parseRequest "https://generativelanguage.googleapis.com/v1/models/gemini-2.0-flash:generateContent"
  let reqContent = RequestContent { reqParts = [RequestPart { reqText = pack promptText }] }
  let genConfig = GenerationConfig { temperature = 0.1, maxOutputTokens = 800 }
  let apiRequest = GeminiApiRequest { contents = [reqContent], generationConfig = genConfig }

  let request = initialRequest
        { requestHeaders =
            [ ("Content-Type", "application/json")
            , ("x-goog-api-key", encodeUtf8 $ pack apiKey)
            ]
        , method = "POST"
        , requestBody = RequestBodyLBS $ Aeson.encode apiRequest
        }

  response <- httpLbs request manager
  let status = responseStatus response
      body = responseBody response

  if statusCode status == 200
    then do
      case eitherDecode body :: Either String GeminiApiResponse of
        Left err -> return $ Left ("Error decoding JSON response: " ++ err)
        Right geminiResponse ->
          case candidates geminiResponse of
            (candidate:_) ->
              case parts (content candidate) of
                (part:_) -> return $ Right (text part)
                [] -> return $ Left "Error: Received candidate with no parts."
            [] ->
              -- Check for blocking information if no candidates are present
              case promptFeedback geminiResponse of
                Just pf -> case blockReason pf of
                             Just reason -> return $ Left ("API Error: Blocked - " ++ reason)
                             Nothing -> return $ Left "Error: No candidates found and no block reason provided."
                Nothing -> return $ Left "Error: No candidates found in response."
    else do
      let err = "Error: API request failed with status " ++ show (statusCode status) ++ "\nBody: " ++ show body
      return $ Left err

-- | Extracts potential place names from text using the Gemini API.
findPlaces :: String             -- ^ Google API Key
           -> Manager            -- ^ HTTP Manager
           -> String             -- ^ The input text to analyze (Renamed from text)
           -> IO (Either String [String]) -- ^ Left error or Right list of places
findPlaces apiKey manager inputText = do
    -- Renamed parameter text -> inputText
    let prompt = "Extract only the place names strictly separated by commas from the following text. Do not include any explanation or introduction. Example: London,Paris,Tokyo\n\nText:\"" ++ inputText ++ "\""
    apiResult <- completion apiKey manager prompt

    return $ case apiResult of
        Left err -> Left ("API call failed in findPlaces: " ++ err)
        Right responseText ->
            let -- Use Text functions directly (removed T. prefix)
                rawParts = splitOn (pack ",") (pack responseText)
                strippedParts = map strip rawParts
                nonEmptyParts = filter (not . Data.Text.null) strippedParts -- Use Data.Text.null to be explicit if Text type is inferred
                -- Convert final list back to [String]
                places = map unpack nonEmptyParts
            in Right places

-- | Extracts potential person names from text using the Gemini API.
findPeople :: String             -- ^ Google API Key
           -> Manager            -- ^ HTTP Manager
           -> String             -- ^ The input text to analyze (Renamed from text)
           -> IO (Either String [String]) -- ^ Left error or Right list of people
findPeople apiKey manager inputText = do
    -- Renamed parameter text -> inputText
    let prompt = "Extract only the person names strictly separated by commas from the following text. Do not include any explanation or introduction. Example: Alice,Bob,Charlie\n\nText:\"" ++ inputText ++ "\""
    apiResult <- completion apiKey manager prompt

    return $ case apiResult of
        Left err -> Left ("API call failed in findPeople: " ++ err)
        Right responseText ->
            let -- Use Text functions directly (removed T. prefix)
                rawParts = splitOn (pack ",") (pack responseText)
                strippedParts = map strip rawParts
                nonEmptyParts = filter (not . Data.Text.null) strippedParts -- Use Data.Text.null to be explicit
                people = map unpack nonEmptyParts
            in Right people

-- | Extracts potential company names from text using the Gemini API.
findCompanyNames :: String             -- ^ Google API Key
                 -> Manager            -- ^ HTTP Manager
                 -> String             -- ^ The input text to analyze
                 -> IO (Either String [String]) -- ^ Left error or Right list of companies
findCompanyNames apiKey manager inputText = do
    let prompt = "Extract only the company names strictly separated by commas from the following text. Do not include any explanation or introduction. Example: Google,Apple,Microsoft\n\nText:\"" ++ inputText ++ "\""
    apiResult <- completion apiKey manager prompt

    return $ case apiResult of
        Left err -> Left ("API call failed in findCompanyNames: " ++ err)
        Right responseText ->
            let
                rawParts = splitOn (pack ",") (pack responseText)
                strippedParts = map strip rawParts
                nonEmptyParts = filter (not . Data.Text.null) strippedParts
                companies = map unpack nonEmptyParts
            in Right companies

-- --- Main Function ---

main :: IO ()
main = do
  args <- getArgs
  case args of
    -- If no args, run demo extraction on sample text
    [] -> do
        putStrLn "No prompt provided. Running demo extraction on sample text:"
        let sampleText = "Dr. Evelyn Reed from Acme Corporation went to London last week with her colleague Bob Smith. They visited the Tower Bridge and met someone near Paris, Texas."
        putStrLn $ "Sample Text: \"" ++ sampleText ++ "\"\n"

        apiKeyResult <- lookupEnv "GOOGLE_API_KEY"
        case apiKeyResult of
            Nothing -> putStrLn "Error: GOOGLE_API_KEY environment variable not set."
            Just apiKey -> do
                manager <- newManager tlsManagerSettings

                -- Find Places
                putStrLn "Attempting to find places..."
                placesResult <- findPlaces apiKey manager sampleText
                case placesResult of
                    Left err -> putStrLn $ "Error finding places: " ++ err
                    Right places -> putStrLn $ "Found Places: " ++ show places

                putStrLn "\nAttempting to find people..."
                -- Find People
                peopleResult <- findPeople apiKey manager sampleText
                case peopleResult of
                    Left err -> putStrLn $ "Error finding people: " ++ err
                    Right people -> putStrLn $ "Found People: " ++ show people

                putStrLn "\nAttempting to find company names..."
                -- Find Companies
                companiesResult <- findCompanyNames apiKey manager sampleText
                case companiesResult of
                    Left err -> putStrLn $ "Error finding companies: " ++ err
                    Right companies -> putStrLn $ "Found Companies: " ++ show companies

    -- If args provided, run original completion behavior
    (promptArg:_) -> do
      putStrLn "Prompt provided. Running direct completion:"
      apiKeyResult <- lookupEnv "GOOGLE_API_KEY"
      case apiKeyResult of
        Nothing -> putStrLn "Error: GOOGLE_API_KEY environment variable not set."
        Just apiKey -> do
          manager <- newManager tlsManagerSettings
          result <- completion apiKey manager promptArg

          case result of
            Left errMsg -> putStrLn $ "API Call Failed:\n" ++ errMsg
            Right completionText -> putStrLn $ "Response:\n\n" ++ completionText


-- Helper function (optional but good practice)
lookupEnv :: String -> IO (Maybe String)
lookupEnv name = handle (\(_ :: SomeException) -> return Nothing) $ Just <$> getEnv name