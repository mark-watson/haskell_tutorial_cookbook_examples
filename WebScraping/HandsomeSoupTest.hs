{-# LANGUAGE OverloadedStrings #-}

-- references: https://github.com/egonSchiele/HandsomeSoup
--             http://adit.io/posts/2012-04-14-working_with_HTML_in_haskell.html

module HandsomeSoupTest where

import Text.XML.HXT.Core
import Text.HandsomeSoup


main :: IO ()
main = do
  let doc = fromUrl "http://markwatson.com/"
  putStrLn "\n\n ** LINKS:\n"
  links <- runX $ doc >>> css "a" ! "href"
  mapM_ putStrLn links
  h2 <- runX $ doc >>> css "h2" //> getText
  putStrLn "\n\n ** ALL H2 ELEMENTS:\n"
  mapM_ putStrLn h2
  imageSrc <- runX $ doc >>> css "img" ! "src"
  putStrLn "\n\n ** ALL IMG ELEMENTS:\n"
  mapM_ putStrLn imageSrc
  allBodyText <- runX $ doc >>> css "body" //> getText
  putStrLn "\n\n ** TEXT FROM BODY ELEMENT:\n"
  mapM_ putStrLn allBodyText
  pText <- runX $ doc >>> css "p" //> getText -- //> gets all contained text
                                              -- /> gets only directly contained text
  putStrLn "\n\n ** ALL P ELEMENTS:\n"
  mapM_ putStrLn pText
  
