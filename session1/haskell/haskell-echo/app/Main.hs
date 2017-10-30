{-# LANGUAGE OverloadedStrings #-}

module Main where

import GHC.IO.Handle (Handle, hSetBuffering, BufferMode(..), hClose)
import Network
import Control.Concurrent
import Text.Printf
import Data.ByteString.Char8 

port = 5437

main :: IO ()
main = withSocketsDo $
    listenOn (PortNumber port) >>= \listenSock -> 
    serverLoop listenSock

serverLoop :: Socket -> IO ()
serverLoop socket =
    accept socket >>= \(handle, host, port) ->
    printf "Hello visitor from %s:%d\n" host (toInteger port) >>
    hSetBuffering handle NoBuffering >>
    forkIO (handleSingleConnection handle) >>= \_threadId ->
    serverLoop socket
   
handleSingleConnection :: Handle -> IO () 
handleSingleConnection handle =
    hGetLine handle >>= \text ->
    if text == "exit\r" then 
      hClose handle
    else
      hPutStrLn handle text >>
      handleSingleConnection handle


