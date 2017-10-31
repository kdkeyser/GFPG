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

withdraw :: String -> Integer -> Accounts -> STM Bool
withdraw name amount accounts =
  case Map.lookup name accounts of
    Just account -> do
      currentValue <- readTVar account
      if currentValue < amount then
        return False
      else do
        writeTVar account $ currentValue - amount
        return True
    Nothing -> return False

deposit :: String -> Integer -> Accounts -> STM Bool
deposit name amount accounts =
  case Map.lookup name accounts of
    Just account -> do
      currentValue <- readTVar account
      writeTVar account $ currentValue + amount
      return True
    Nothing -> return False

data Wire = Wire String String Integer

wire :: Wire -> Accounts -> IO Bool 
wire (Wire source destination value) accounts = do
  atomically $ do
    withdrawSuccess <- withdraw source value accounts
    if withdrawSuccess then do
      deposit destination value accounts
      return True
    else
      return False

randomWire :: Accounts -> IO Wire
randomWire accounts = do
  value <- randomRIO (1, 100)
  from <- randomRIO (0,2)
  to <- randomRIO (0,1)
  let (fromName,_) = Map.elemAt from accounts
  let (toName,_) = Map.elemAt to $ Map.deleteAt from accounts
  return $ Wire fromName toName value

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

main :: IO ()
main = do
  accounts <- atomically initialAccounts
  printAccounts accounts
  threads <- sequence $ replicate 1000 $ do
    async $ do
      sequence $ replicate 1000 $ do
        wireInfo <- randomWire accounts
        wire wireInfo accounts
  sequence $ fmap wait $ threads
  printAccounts accounts 
