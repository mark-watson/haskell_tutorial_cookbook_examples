{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DeriveGeneric #-} -- Added DeriveGeneric for clarity, though Generic is imported

import Control.Monad (when) -- Import when
import System.Environment (getArgs)
import qualified Data.Aeson as Aeson
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic) -- Explicitly import Generic
import Network.HTTP.Client
  ( newManager
  , httpLbs
  , parseRequest
  , Request(..)
  , RequestBody(..)
  , responseBody
  , responseStatus
  , defaultManagerSettings
  , Manager -- Import Manager type
  )
import Network.HTTP.Types.Status (statusIsSuccessful) -- Import statusIsSuccessful

-- Data types for Ollama interaction
data OllamaRequest = OllamaRequest
  { model :: String
  , prompt :: String
  , stream :: Bool
  } deriving (Show, Generic, ToJSON) -- Derive Generic and ToJSON

data OllamaResponse = OllamaResponse
  { model :: String
  , created_at :: String
  , response :: String -- This matches the actual field name in the JSON
  , done :: Bool
  , done_reason :: Maybe String -- done_reason might be null/missing in some responses, using Maybe is safer
  } deriving (Show, Generic, FromJSON) -- Derive Generic and FromJSON

-- Function to call the Ollama API
callOllama :: Manager -> String -> String -> IO (Either String OllamaResponse)
callOllama manager modelName userPrompt = do
  -- Note: parseRequest throws exceptions on invalid URLs, which is acceptable here.
  initialRequest <- parseRequest "http://localhost:11434/api/generate"

  let ollamaRequestBody = OllamaRequest
        { model = modelName
        , prompt = userPrompt
        , stream = False -- Keeping stream as False for a single response
        }

  let request = initialRequest
        { requestHeaders = [("Content-Type", "application/json")]
        , method = "POST"
        , requestBody = RequestBodyLBS $ Aeson.encode ollamaRequestBody
        }

  -- httpLbs also throws exceptions on network errors, which `main` handles implicitly
  httpResponse <- httpLbs request manager

  let status = responseStatus httpResponse
      body = responseBody httpResponse

  if statusIsSuccessful status -- Use statusIsSuccessful for clarity
    then do
      let maybeOllamaResponse = Aeson.decode body :: Maybe OllamaResponse
      case maybeOllamaResponse of
        Just ollamaResponse -> return $ Right ollamaResponse
        Nothing -> return $ Left $ "Error: Failed to parse JSON response. Body: " ++ show body
    else do
      return $ Left $ "Error: HTTP request failed with status " ++ show status ++ ". Body: " ++ show body

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Usage: <program_name> <prompt> [model_name]"
    (promptArg:modelArgs) -> do
      let modelName = case modelArgs of
                        (m:_) -> m
                        []    -> "llama3.2:latest" -- Default model

      manager <- newManager defaultManagerSettings

      putStrLn $ "Sending prompt '" ++ promptArg ++ "' to model '" ++ modelName ++ "'..."

      result <- callOllama manager modelName promptArg

      case result of
        Right ollamaResponse -> do
          -- No need for liftIO here, putStrLn is already IO
          putStrLn "\n--- Response ---"
          putStrLn ollamaResponse.response
          when (ollamaResponse.done_reason /= Nothing) $ -- Check if done_reason is present
              putStrLn $ "\nDone reason: " ++ show ollamaResponse.done_reason -- Show the reason if present
        Left err -> do
          -- No need for liftIO here either
          putStrLn $ "API Error: " ++ err