import Control.Monad.IO.Class (liftIO)
import System.Environment (getArgs, getEnv)
import qualified Data.Aeson as Aeson
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Client (newManager, httpLbs, parseRequest, Request(..), RequestBody(..), responseBody, responseStatus)
import Network.HTTP.Types.Status (statusCode)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Vector as V

data GeminiRequest = GeminiRequest
  { prompt :: String
  } deriving (Show, Generic, ToJSON)

data GeminiResponse = GeminiResponse
  { candidates :: [Candidate]  -- Changed from choices to candidates
  } deriving (Show, Generic, FromJSON)

data Candidate = Candidate
  { content :: Content
  } deriving (Show, Generic, FromJSON)

data Content = Content
  { parts :: [Part]
  } deriving (Show, Generic, FromJSON)

data Part = Part
  { text :: String
  } deriving (Show, Generic, FromJSON, ToJSON)

data PromptFeedback = PromptFeedback
  { blockReason :: Maybe String
  , safetyRatings :: Maybe [SafetyRating]
  } deriving (Show, Generic, FromJSON, ToJSON)

data SafetyRating = SafetyRating
  { category :: String
  , probability :: String
  } deriving (Show, Generic, FromJSON, ToJSON)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Error: Please provide a prompt as a command line argument."
    (arg:_) -> do  --  Extract the argument directly
      apiKey <- getEnv "GOOGLE_API_KEY"

      manager <- newManager tlsManagerSettings

      initialRequest <- parseRequest "https://generativelanguage.googleapis.com/v1/models/gemini-1.5-pro:generateContent"

      let geminiRequestBody = Aeson.object [
            ("contents", Aeson.Array $ V.singleton $ Aeson.object [
                ("parts", Aeson.Array $ V.singleton $ Aeson.object [
                    ("text", Aeson.String $ T.pack  arg)
                    ])
            ]),
            ("generationConfig", Aeson.object [
                ("temperature", Aeson.Number 0.1),
                ("maxOutputTokens", Aeson.Number 800)
                ])
            ]
            
      let request = initialRequest
            { requestHeaders =
                [ ("Content-Type", "application/json")
                , ("x-goog-api-key", encodeUtf8 $ T.pack apiKey)
                ]
            , method = "POST"
            , requestBody = RequestBodyLBS $ Aeson.encode geminiRequestBody
            }

      response <- httpLbs request manager
      
      let responseStatus' = responseStatus response

      if statusCode responseStatus' == 200
        then do
        let maybeGeminiResponse = Aeson.decode (responseBody response) :: Maybe GeminiResponse
        case maybeGeminiResponse of
          Just geminiResponse -> do
            case candidates geminiResponse of
              (candidate:_) -> do
                case parts (content candidate) of
                  (part:_) -> do  -- Changed text_ to _ since it's unused
                    liftIO $ putStrLn $ "Response:\n\n" ++ text part
                  [] -> do
                    liftIO $ putStrLn "Error: No parts in content"
              [] -> do
                liftIO $ putStrLn "Error: No candidates in response"
          Nothing -> do
             liftIO $ putStrLn "Error: Failed to parse response"
        else do
        putStrLn $ "Error: " ++ show responseStatus'
