{-# LANGUAGE OverloadedStrings #-}
import OpenAI.Client

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import System.Environment (getEnv)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import Data.Text (splitOn)

-- example derived from the openai-client library documentation

completionRequestToString :: String -> IO String
completionRequestToString prompt = do
    manager <- newManager tlsManagerSettings
    apiKey <- T.pack <$> getEnv "OPENAI_KEY"
    let client = makeOpenAIClient apiKey manager 4
    let request = ChatCompletionRequest
                 { chcrModel = ModelId "gpt-4o"
                 , chcrMessages =
                    [ ChatMessage
                        { chmContent = Just (T.pack prompt)
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
    result <- completeChat client request
    case result of
        Left failure -> return (show failure)
        Right success ->
            case chrChoices success of
                (ChatChoice {chchMessage = ChatMessage {chmContent = content}} : _) ->
                    return $ fromMaybe "No content" $ T.unpack <$> content
                _ -> return "No choices returned"

-- find place names
findPlaces :: String -> IO [String]
findPlaces text = do
    let prompt = "Extract only the place names separated by commas from the following text:\n\n" ++ text
    response <- completionRequestToString prompt 
    -- Convert Text to String using T.unpack before filtering
    let places = filter (not . null) $ map T.unpack $ splitOn "," (T.pack response) 
    -- Strip leading and trailing whitespace from each place name
    return $ map (T.unpack . T.strip . T.pack) places

main :: IO ()
main = do
    response <- completionRequestToString "Write a hello world program in Haskell"
    putStrLn response

    places <- findPlaces "I visited London, Paris, and New York last year."
    print places 