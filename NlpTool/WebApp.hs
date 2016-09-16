-- Copyright 2014 by Mark Watson. All rights reserved. The software and data in this project can be used under the terms of the GPL version 3 license.


{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

import Yesod
import qualified Data.Text as T
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
--import Control.Monad (liftM)
--import Control.Monad.Exception.Synchronous (catch, runExceptionalT, Exceptional(Exception), Exceptional(Success))
import Yesod.Core.Types (Logger)

import System.IO (readLn)
import Utils (splitWordsKeepCase, cleanText)

import Categorize
import Entities
import Summarize
--import OpenCalais

data App = App

mkYesod "App" [parseRoutes|
/         HomeR     GET POST
|]

instance Yesod App

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

readSession :: Read a => T.Text -> Handler (Maybe a)
readSession name = do
    textValue <- lookupSession name
    return (readMaybe . T.unpack =<< textValue)

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  setTitle "Haskell Categorization Demo"
  categories <- lookupSession "categories"
  humanNames <- lookupSession "humanNames"
  countryNames <- lookupSession "countryNames"
  cityNames <- lookupSession "cityNames"
  companyNames <- lookupSession "companyNames"
  broadcastNetworkNames <- lookupSession "broadcastNetworkNames"
  politicalPartyNames <- lookupSession "politicalPartyNames"
  tradeUnionNames <- lookupSession "tradeUnionNames"
  universityNames <- lookupSession "universityNames"
  summary <- lookupSession "summary"
  summary_s <- lookupSession "summary_s"
  --calais <- lookupSession "calais"
  the_text <- lookupSession "the_text"
  deleteSession "categories"
  deleteSession "humanNames"
  deleteSession "countryNames"
  deleteSession "cityNames"
  deleteSession "companyNames"
  deleteSession "broadcastNetworkNames"
  deleteSession "politicalPartyNames"
  deleteSession "tradeUnionNames"
  deleteSession "universityNames"
  deleteSession "summary"
  deleteSession "summary_s"
  --deleteSession "calais"
  deleteSession "the_text"
  toWidget [lucius|
            body { margin: 2em; }
            h2 { background-color: #C0C0C0; padding: 2em; -moz-border-radius: 15px; border-radius: 15px;}
            form { background-color: #e0e0e0; padding: 1em; -moz-border-radius: 15px; border-radius: 15px;}
            textarea { background-color: #f2f2f2; padding: 1em; -moz-border-radius: 15px; border-radius: 15px; width: 95%; }
            input { background-color: #C0C0C0; padding: 1em; -moz-border-radius: 15px; border-radius: 15px;}
   |]
  [whamlet|
     <h2>This is a test of an initial port of KnowledgeBooks Natural Language Processing (NLP) code to Haskell
     <p>This system attempts to resolve entity references to Wikipedia/DBPedia subject URIs.
     <h4>Enter plain text (no special characters):
     <form method=post>
        <textarea type=text name=name rows="6" cols="70">
        <br>
        <br>
        <input type=submit value="Process text">
     <br>
     <p>
       <i>Note: if you don't see any results the cause is special characters (e.g., fancy quotes and other unicode characters) in the input text.
     <p>#{fromMaybe "" the_text}
     <h4>Summary of text:
     <p>#{fromMaybe "" summary_s}
     <h4>Category results from combined 1gram and 2gram analysis:
     <p>#{fromMaybe "" categories}
     <h4>Human names found in text:
     <p>#{fromMaybe "" humanNames}
     <h4>Country names found in text:
     <p>#{fromMaybe "" countryNames}
     <h4>City names found in text:
     <p>#{fromMaybe "" cityNames}
     <h4>Company names found in text:
     <p>#{fromMaybe "" companyNames}
     <h4>Braodcast network names found in text:
     <p>#{fromMaybe "" broadcastNetworkNames}
     <h4>Political party names found in text:
     <p>#{fromMaybe "" politicalPartyNames}
     <h4>Trade union names found in text:
     <p>#{fromMaybe "" tradeUnionNames}
     <h4>University names found in text:
     <p>#{fromMaybe "" universityNames}
     <h4>Summary of text, with scoring:
     <p>#{fromMaybe "" summary}
     <br>
     <br>
     <div>
       <p>This is just demo code. Please see my commercial NLP product at <a href="http://kbsportal.com">kbsportal.com
       <i>Copyright 2014-2015 Mark Watson.
   |]

postHomeR :: Handler ()
postHomeR = do
    name <- runInputPost $ ireq textField "name"
    --calais <- lift $ calaisResults $ T.unpack name

    setSession "categories" $ T.pack $ (show $ bestCategories $ splitWords $ cleanText $ T.unpack name)
    setSession "humanNames" $ T.pack $ (show $ peopleNames $ splitWordsKeepCase $ cleanText $ T.unpack name)
    setSession "countryNames" $ T.pack $ (show $ countryNames $ splitWordsKeepCase $ cleanText $ T.unpack name)
    setSession "cityNames" $ T.pack $ (show $ cityNames $ splitWordsKeepCase $ cleanText $ T.unpack name)
    setSession "companyNames" $ T.pack $ (show $ companyNames $ splitWordsKeepCase $ cleanText $ T.unpack name)
    setSession "broadcastNetworkNames" $ T.pack $ (show $ broadcastNetworkNames $ splitWordsKeepCase $ cleanText $ T.unpack name)
    setSession "politicalPartyNames" $ T.pack $ (show $ politicalPartyNames $ splitWordsKeepCase $ cleanText $ T.unpack name)
    setSession "tradeUnionNames" $ T.pack $ (show $ tradeUnionNames $ splitWordsKeepCase $ cleanText $ T.unpack name)
    setSession "universityNames" $ T.pack $ (show $ universityNames $ splitWordsKeepCase $ cleanText $ T.unpack name)
    setSession "summary" $ T.pack $ (show $ summarize $ cleanText $ T.unpack name)
    --setSession "calais"  $ T.pack calais
    setSession "summary_s" $ T.pack $ (show $ summarize_s $ cleanText $ T.unpack name)
    setSession "the_text" name
    redirectUltDest HomeR

main :: IO ()
main = warp 3000 App
