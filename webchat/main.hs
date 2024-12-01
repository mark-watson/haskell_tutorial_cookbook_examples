import Web.Scotty
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Client
import Network.HTTP.Types.Status (status500)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Lazy as TL
import qualified Data.Aeson as Aeson
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics
import qualified Data.Text as T
import System.IO (hFlush, stdout)
import System.Environment (getEnv)
import qualified Data.Vector as V

data GeminiRequest = GeminiRequest
  { prompt :: String
  } deriving (Show, Generic, FromJSON, ToJSON)

data GeminiResponse = GeminiResponse
  { candidates :: [Candidate]
  , promptFeedback :: Maybe PromptFeedback
  } deriving (Show, Generic, FromJSON, ToJSON)

data Candidate = Candidate
  { content :: Content2
  , finishReason :: Maybe String
  , index :: Maybe Int
  } deriving (Show, Generic, FromJSON, ToJSON)

data Content2 = Content2
  { parts :: [Part]
  , role :: Maybe String
  } deriving (Show, Generic, FromJSON, ToJSON)

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
  apiKey <- getEnv "GOOGLE_API_KEY"
  scotty 3000 $ do
    get "/" $ do
      html $ TL.pack
        "<!DOCTYPE html>\
        \<html>\
        \<head>\
        \<title>Gemini Chat</title>\
        \</head>\
        \<body>\
        \  <h1>Gemini Chat</h1>\
        \  <form id='chat-form'>\
        \    <input type='text' id='prompt' name='prompt' placeholder='Enter your prompt'\
        \           style='width: 70%; padding: 10px; font-size: 16px;'>\
        \    <button type='submit'>Send</button>\
        \  </form><br/><br/><h4>Response:</h4>\
        \  <div id='response'></div>\
        \  <script>\
        \   const form = document.getElementById('chat-form');\
        \   const responseDiv = document.getElementById('response');\
        \   form.addEventListener('submit', async (event) => {\
        \     event.preventDefault();\
        \     const prompt = document.getElementById('prompt').value;\
        \     try {\
        \       const response = await fetch('/chat', {\
        \         method: 'POST',\
        \         headers: { 'Content-Type': 'application/json' },\
        \         body: JSON.stringify({ prompt: prompt })\
        \       });\
        \       const data = await response.json();\
        \       responseDiv.innerText = data.text;\
        \     } catch (error) {\
        \       console.error('Error:', error);\
        \       responseDiv.innerText = 'Error occurred while fetching response';\
        \     }\
        \   });\
        \  </script>\
        \</body>\
        \</html>"

    post "/chat" $ do
      req <- jsonData :: ActionM GeminiRequest
      liftIO $ putStrLn $ "Received request: " ++ show req
      liftIO $ hFlush stdout

      manager <- liftIO $ newManager tlsManagerSettings

      initialRequest <- liftIO $ parseRequest 
        "https://generativelanguage.googleapis.com/v1/models/gemini-pro:generateContent"

      let geminiRequestBody = Aeson.object
            [ ("contents", Aeson.Array $ V.singleton $ Aeson.object
                [ ("parts", Aeson.Array $ V.singleton $ Aeson.object
                    [ ("text", Aeson.String $ T.pack $ prompt req)
                    ]
                  )
                ]
              )
            , ("generationConfig", Aeson.object
                [ ("temperature", Aeson.Number 0.1)
                , ("maxOutputTokens", Aeson.Number 800)
                ]
              )
            ]

      let request2 = initialRequest
            { requestHeaders =
                [ ("Content-Type", "application/json")
                , ("x-goog-api-key", BS.pack apiKey)
                ]
            , method = "POST"
            , requestBody = RequestBodyLBS $ Aeson.encode geminiRequestBody
            }

      liftIO $ putStrLn $ "Request body: " ++ show (Aeson.encode geminiRequestBody)
      liftIO $ hFlush stdout

      response2 <- liftIO $ httpLbs request2 manager
      liftIO $ do
        putStrLn $ "Response status: " ++ show (responseStatus response2)
        putStrLn $ "Response headers: " ++ show (responseHeaders response2)
        putStrLn $ "Raw response: " ++ show (responseBody response2)
        hFlush stdout

      let maybeGeminiResponse = Aeson.decode (responseBody response2) :: Maybe GeminiResponse
      
      liftIO $ putStrLn $ "Parsed response: " ++ show maybeGeminiResponse  -- Debug print
      liftIO $ hFlush stdout

      case maybeGeminiResponse of
        Just geminiResponse -> do
          case candidates geminiResponse of
            (candidate:_) -> do
              case parts (content candidate) of
                (part:_) -> do
                  liftIO $ putStrLn $ "Sending response: " ++ show part
                  liftIO $ hFlush stdout
                  json part
                [] -> do
                  liftIO $ putStrLn "No parts in response"
                  status status500 >> Web.Scotty.text "No content in response"
            [] -> do
              liftIO $ putStrLn "No candidates in response"
              status status500 >> Web.Scotty.text "No candidates in response"
        Nothing -> do
          liftIO $ putStrLn "Failed to parse response"
          status status500 >> Web.Scotty.text "Failed to parse Gemini response"
