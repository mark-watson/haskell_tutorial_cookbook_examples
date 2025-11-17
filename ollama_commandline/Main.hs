-- Simple command-line client for Ollama's local API
-- Usage: run as `Main "<prompt>" [model]` or `runghc Main.hs "<prompt>" [model]`. Default model: `llama3.2:latest`
-- LANGUAGE pragmas enable features used below
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DeriveGeneric #-}

-- Core utilities
import Control.Monad (when)
import System.Environment (getArgs)

-- JSON support
import qualified Data.Aeson as Aeson
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

-- HTTP client
import Network.HTTP.Client
  ( newManager
  , httpLbs
  , parseRequest
  , Request(..)
  , RequestBody(..)
  , responseBody
  , responseStatus
  , defaultManagerSettings
  , Manager
  )
import Network.HTTP.Types.Status (statusIsSuccessful)

-- Types that mirror Ollama's request and response JSON
data OllamaRequest = OllamaRequest
  { model :: String        -- name/tag of the model to use
  , prompt :: String       -- user input sent to the model
  , stream :: Bool         -- stream tokens or return a single final string
  } deriving (Show, Generic, ToJSON)

data OllamaResponse = OllamaResponse
  { model :: String
  , created_at :: String
  , response :: String     -- the generated text from the model
  , done :: Bool
  , done_reason :: Maybe String -- may be missing; use Maybe
  } deriving (Show, Generic, FromJSON)

-- Call Ollama's local API and decode the JSON response
callOllama :: Manager -> String -> String -> IO (Either String OllamaResponse)
callOllama manager modelName userPrompt = do
  -- Build the POST request to /api/generate
  initialRequest <- parseRequest "http://localhost:11434/api/generate"

  let ollamaRequestBody = OllamaRequest
        { model = modelName
        , prompt = userPrompt
        , stream = False     -- single complete response
        }

  let request = initialRequest
        { requestHeaders = [("Content-Type", "application/json")]
        , method = "POST"
        , requestBody = RequestBodyLBS $ Aeson.encode ollamaRequestBody -- encode as JSON
        }

  -- Send the request and get the HTTP response
  httpResponse <- httpLbs request manager

  let status = responseStatus httpResponse
      body = responseBody httpResponse

  if statusIsSuccessful status
    then do
      -- Try to decode the JSON body into our Haskell type
      let maybeOllamaResponse = Aeson.decode body :: Maybe OllamaResponse
      case maybeOllamaResponse of
        Just ollamaResponse -> return $ Right ollamaResponse
        Nothing -> return $ Left $ "Error: Failed to parse JSON response. Body: " ++ show body
    else
      -- Non-2xx HTTP status
      return $ Left $ "Error: HTTP request failed with status " ++ show status ++ ". Body: " ++ show body

main :: IO ()
main = do
  -- Read command-line args: prompt and optional model name
  args <- getArgs
  case args of
    [] -> putStrLn "Usage: <program_name> <prompt> [model_name]"
    (promptArg:modelArgs) -> do
      -- Choose model: use user-provided or default
      let modelName = case modelArgs of
                        (m:_) -> m
                        []    -> "llama3.2:latest"

      -- Create an HTTP connection manager
      manager <- newManager defaultManagerSettings

      putStrLn $ "Sending prompt '" ++ promptArg ++ "' to model '" ++ modelName ++ "'..."

      -- Make the API call
      result <- callOllama manager modelName promptArg

      -- Handle success or error
      case result of
        Right ollamaResponse -> do
          putStrLn "\n--- Response ---"
          putStrLn ollamaResponse.response
          -- Print reason if present
          when (ollamaResponse.done_reason /= Nothing) $
              putStrLn $ "\nDone reason: " ++ show ollamaResponse.done_reason
        Left err -> do
          putStrLn $ "API Error: " ++ err