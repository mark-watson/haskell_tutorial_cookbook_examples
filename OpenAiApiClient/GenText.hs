-- Example from https://github.com/agrafix/openai-hs/tree/main/openai-hs
-- code by Alexander Thiemann

{-# LANGUAGE OverloadedStrings #-}
import OpenAI.Client

import Network.HTTP.Client

import Network.HTTP.Client.TLS
import System.Environment (getEnv)
import qualified Data.Text as T

request :: ChatCompletionRequest
request = ChatCompletionRequest 
         { chcrModel = ModelId "gpt-4o"
         , chcrMessages = 
            [ChatMessage { chmContent = Just "Write a hello world program in Haskell"
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

main :: IO ()
main =
  do manager <- newManager tlsManagerSettings
     apiKey <- T.pack <$> getEnv "OPENAI_KEY"
     -- create a openai client that automatically retries up to 4 times on network
     -- errors
     let client = makeOpenAIClient apiKey manager 4
     result <- completeChat client request        
     case result of
       Left failure -> print failure
       Right success -> print $ chrChoices success
