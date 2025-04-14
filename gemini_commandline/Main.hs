{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import System.Environment (getArgs, getEnv)
import qualified Data.Aeson as Aeson -- Used for Aeson.encode, Aeson.object etc.
import Data.Aeson (FromJSON, ToJSON, eitherDecode) -- Specific functions needed
import GHC.Generics (Generic) -- Needed for deriving ToJSON/FromJSON
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Client (newManager, httpLbs, parseRequest, Manager, Request(..), RequestBody(..), Response(..), responseStatus)
import Network.HTTP.Types.Status (statusCode)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Control.Exception (SomeException, handle)

-- --- Request Data Types ---

data RequestPart = RequestPart
  { reqText :: T.Text  -- Using reqText to avoid name clash with Response Part's text
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
  let reqContent = RequestContent { reqParts = [RequestPart { reqText = T.pack promptText }] }
  let genConfig = GenerationConfig { temperature = 0.1, maxOutputTokens = 800 }
  let apiRequest = GeminiApiRequest { contents = [reqContent], generationConfig = genConfig }

  let request = initialRequest
        { requestHeaders =
            [ ("Content-Type", "application/json")
            , ("x-goog-api-key", encodeUtf8 $ T.pack apiKey)
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

-- --- Main Function ---

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Error: Please provide a prompt as a command line argument."
    (promptArg:_) -> do
      apiKeyResult <- lookupEnv "GOOGLE_API_KEY" -- Using lookupEnv for safer handling
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