{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import qualified Network.Simple.TCP as T

main = do
  T.connect "127.0.0.1" "3000" $ \(connectionSocket, remoteAddr) -> do
    putStrLn $ "Connection established to " ++ show remoteAddr
    T.send connectionSocket "test123"
    response <- T.recv connectionSocket 100
    case response of
      Just s -> putStrLn $ "Response: " ++ show s
      Nothing -> putStrLn "No response from server"

