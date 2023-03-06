{-# LANGUAGE OverloadedStrings #-}
import           Data.Aeson            (Value)
import qualified Data.ByteString.Char8 as S8
import qualified Data.Yaml             as Yaml
import           Network.HTTP.Simple

import Data.Typeable(typeOf) -- for debugging
import Control.Monad.Trans(lift) -- experiment only

main :: IO ()
main = do

    response <- httpJSON "http://dbpedia.org/sparql/?query=select * where {?s ?p ?o} limit 2"
    print $ typeOf response
--    rb <- lift $ (getResponseBody response) --  :: Value)
    putStrLn $ "The status code was: " ++
               show (getResponseStatusCode response)
    print $ getResponseHeader "Content-Type" response
    S8.putStrLn $ Yaml.encode (getResponseBody response :: Value)
 


