{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Control.Concurrent.Async
import Control.Concurrent.STM
import Data.Map (Map)
import qualified Data.Map as Map
import System.Random

type Account = TVar Integer

type Accounts = Map String Account

createAccount :: String -> Integer -> STM (String, Account)
createAccount name value = do
  account <- newTVar value
  return (name, account)

initialAccounts :: STM Accounts
initialAccounts = do
  mark <- createAccount "Mark" 40000
  kasper <- createAccount "Kasper" 35000
  koen <- createAccount "Koen" 21000
  return $ Map.fromList [mark,kasper,koen]

printAccounts :: Accounts -> IO ()
printAccounts accounts = do
  let names = fmap fst $ Map.toList accounts
  values <- atomically $ sequence $ fmap (readTVar . snd) $ Map.toList accounts
  let total = sum values
  putStrLn $ show $ zip names values
  putStrLn $ "Total = " ++ show total

withdraw :: String -> Integer -> Accounts -> STM ()
withdraw name amount accounts =
  case Map.lookup name accounts of
    Just account -> do
      currentValue <- readTVar account
      writeTVar account $ currentValue - amount
    Nothing -> return ()

deposit :: String -> Integer -> Accounts -> STM ()
deposit name amount accounts =
  case Map.lookup name accounts of
    Just account -> do
      currentValue <- readTVar account
      writeTVar account $ currentValue + amount
    Nothing -> return ()

data Wire = Wire String String Integer

randomWire :: Accounts -> IO Wire
randomWire accounts = do
  value <- randomRIO (1, 100)
  from <- randomRIO (0,2)
  to <- randomRIO (0,1)
  let (fromName,_) = Map.elemAt from accounts
  let (toName,_) = Map.elemAt to $ Map.deleteAt from accounts
  return $ Wire fromName toName value

applyWire :: Wire -> Accounts -> IO () 
applyWire (Wire source destination value) accounts = do
  atomically $ do
    withdraw source value accounts
    deposit destination value accounts

main :: IO ()
main = do
  accounts <- atomically initialAccounts
  printAccounts accounts
  threads <- sequence $ replicate 100 $ do
    async $ do
      sequence $ replicate 10000 $ do
        wire <- randomWire accounts
        applyWire wire accounts
  sequence $ fmap wait $ threads
  printAccounts accounts 
