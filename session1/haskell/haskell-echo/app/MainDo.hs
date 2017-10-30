{-# LANGUAGE OverloadedStrings #-}

module Main where

import GHC.IO.Handle (Handle, hSetBuffering, BufferMode(..), hClose)
import Network
import Control.Concurrent
import Text.Printf
import Data.ByteString.Char8 

port = 5437

main :: IO ()
main = withSocketsDo $ do
    listenSock <- listenOn $ PortNumber port
    serverLoop listenSock

serverLoop :: Socket -> IO ()
serverLoop socket = do
    (handle, host, port) <- accept socket
    printf "Hello visitor from %s:%d\n" host (toInteger port)
    hSetBuffering handle NoBuffering
    _threadId <- forkIO $ handleSingleConnection handle
    serverLoop socket
   
handleSingleConnection :: Handle -> IO () 
handleSingleConnection handle = do
    text <- hGetLine handle
    if text == "exit\r" then 
      hClose handle
    else do
      hPutStrLn handle text
      handleSingleConnection handle


