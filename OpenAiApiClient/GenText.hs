{-# LANGUAGE OverloadedStrings #-}
import OpenAI.Client

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import System.Environment (getEnv)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)

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

main :: IO ()
main = do
    response <- completionRequestToString "Write a hello world program in Haskell"
    putStrLn response