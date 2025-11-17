{-# LANGUAGE OverloadedStrings #-}
import OpenAI.Client

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import System.Environment (getEnv)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import Data.Text (splitOn)

-- Example using openai-hs for chat completions
-- Requires `OPENAI_KEY` in your environment (e.g., `export OPENAI_KEY=sk-...`)

-- Sends a chat prompt and returns the assistant's text as String
completionRequestToString :: String -> IO String
completionRequestToString prompt = do
    -- Create an HTTPS-capable connection manager
    manager <- newManager tlsManagerSettings
    -- Read your OpenAI API key from the environment
    apiKey <- T.pack <$> getEnv "OPENAI_KEY"
    -- Build a client; the last argument (4) retries on transient network errors
    let client = makeOpenAIClient apiKey manager 4
    -- Describe the chat request to send
    let request = ChatCompletionRequest
                 { chcrModel = ModelId "gpt-5-mini"  -- model to use
                 , chcrMessages =
                    [ ChatMessage
                        { chmContent = Just (T.pack prompt)  -- user prompt
                        , chmRole = "user"
                        , chmFunctionCall = Nothing
                        , chmName = Nothing
                        }
                    ]
                 , chcrFunctions = Nothing
                 , chcrTemperature = Nothing
                 , chcrTopP = Nothing
                 , chcrN = Nothing
                 , chcrStream = Nothing
                 , chcrStop = Nothing
                 , chcrMaxTokens = Nothing
                 , chcrPresencePenalty = Nothing
                 , chcrFrequencyPenalty = Nothing
                 , chcrLogitBias = Nothing
                 , chcrUser = Nothing
                 }
    -- Perform the API call
    result <- completeChat client request
    -- Unpack the result and extract the text content from the first choice
    case result of
        Left failure -> return (show failure)
        Right success ->
            case chrChoices success of
                (ChatChoice {chchMessage = ChatMessage {chmContent = content}} : _) ->
                    return $ fromMaybe "No content" $ T.unpack <$> content
                _ -> return "No choices returned"

-- find place names
-- Extracts place names from `text` (comma-separated) using the chat model
findPlaces :: String -> IO [String]
findPlaces text = do
    -- Construct the extraction prompt
    let prompt = "Extract only the place names separated by commas from the following text:\n\n" ++ text
    response <- completionRequestToString prompt 
    -- Convert Text to String using T.unpack before filtering
    let places = filter (not . null) $ map T.unpack $ splitOn "," (T.pack response) 
    -- Strip leading and trailing whitespace from each place name
    return $ map (T.unpack . T.strip . T.pack) places

-- Extracts person names from `text` (comma-separated) using the chat model
findPeople :: String -> IO [String]
findPeople text = do
    let prompt = "Extract only the person names separated by commas from the following text:\n\n" ++ text
    response <- completionRequestToString prompt
    let people = filter (not . null) $ map T.unpack $ splitOn "," (T.pack response)
    return $ map (T.unpack . T.strip . T.pack) people

-- Demo: generate text, then extract places and people
main :: IO ()
main = do
    -- Generic text generation
    response <- completionRequestToString "Write a hello world program in Haskell"
    putStrLn response

    -- Extract place names
    places <- findPlaces "I visited London, Paris, and New York last year."
    print places

    -- Extract person names
    people <- findPeople "John Smith met with Sarah Johnson and Michael Brown at the conference."
    print people