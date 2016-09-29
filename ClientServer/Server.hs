{-# LANGUAGE OverloadedStrings #-}

module Main where

-- example derived from an network-simple library example

import Control.Monad
import qualified Data.ByteString.Char8 as B
import qualified Network.Simple.TCP as T

reverseStringLoop sock = do
  -- get a byte string wrapped as a MonadIO:
  mbs <- T.recv sock 4096
  case mbs of
    Just bs -> T.send sock (B.reverse bs) >> reverseStringLoop sock
    Nothing -> return ()

main :: IO ()
main = T.withSocketsDo $ do -- derived from library example
  T.listen "*" "3000" $ \(lsock, laddr) -> do
    putStrLn $ "Listening at " ++ show laddr
    forever . T.acceptFork lsock $ \(sock, addr) -> do
      putStrLn $ "Connection from " ++ show addr
      reverseStringLoop sock

