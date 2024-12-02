{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

import Control.Monad.IO.Class (liftIO)
import System.Environment (getArgs)
import qualified Data.Aeson as Aeson
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics
import Network.HTTP.Client (newManager, httpLbs, parseRequest, Request(..), RequestBody(..), responseBody, responseStatus, defaultManagerSettings)
import Network.HTTP.Types.Status (statusCode)
--import qualified Data.Text as T
--import Data.Text.Encoding (encodeUtf8)

data OllamaRequest = OllamaRequest
  { model :: String
  , prompt :: String
  , stream :: Bool
  } deriving (Show, Generic, ToJSON)

data OllamaResponse = OllamaResponse
  { model :: String
  , created_at :: String
  , response :: String  -- This matches the actual field name in the JSON
  , done :: Bool
  , done_reason :: String
  } deriving (Show, Generic, FromJSON)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Error: Please provide a prompt as a command-line argument."
    (arg:_) -> do
      manager <- newManager defaultManagerSettings

      initialRequest <- parseRequest "http://localhost:11434/api/generate"

      let ollamaRequestBody = OllamaRequest
            { model = "llama3.2:latest"  -- You can change this to your preferred model
            , prompt = arg
            , stream = False
            }

      let request = initialRequest
            { requestHeaders = [("Content-Type", "application/json")]
            , method = "POST"
            , requestBody = RequestBodyLBS $ Aeson.encode ollamaRequestBody
            }

      httpResponse <- httpLbs request manager
--    liftIO $ putStrLn $ "httpResponse:" ++ show httpResponse -- debug
      
      let responseStatus' = responseStatus httpResponse

      if statusCode responseStatus' == 200
        then do
          let maybeOllamaResponse =
                Aeson.decode (responseBody httpResponse) :: Maybe OllamaResponse
          case maybeOllamaResponse of
            Just ollamaResponse -> do
              liftIO $ putStrLn $ "Response:\n\n" ++ ollamaResponse.response
            Nothing -> do
              liftIO $ putStrLn "Error: Failed to parse response"
        else do
          putStrLn $ "Error: " ++ show responseStatus'
