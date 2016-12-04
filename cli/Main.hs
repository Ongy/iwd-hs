#!/usr/bin/runhaskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main
where

import Network.IWD.Types
import Network.IWD.Query
import Network.IWD.Agent
import Data.List (find)
import System.Environment (getArgs)
import Control.Monad (when, zipWithM_)
import DBus
import DBus.Client


printNet :: Maybe ObjectPath -> Network -> IO ()
printNet connected net = do
    putStr (networkName net)
    when (connected == (Just . networkPath $ net)) (putStr "*")
    putStr "  "
    putStrLn (networkType net)

printDevice :: Device -> [Network] -> IO ()
printDevice device nets = do
    putStr (deviceName device)
    putStr "  "
    putStrLn (deviceState device)

    mapM_ (printNet (deviceConNetwork device)) nets
    putStrLn "\n"

printDevices :: IO ()
printDevices = do
    bus <- connectSystem

    devs <- getDevices bus
    nets <- mapM (getNetworks bus) devs

    zipWithM_ printDevice devs nets

doScan :: String -> IO ()
doScan devName = do
    bus <- connectSystem

    dev <- findDevice bus devName
    case dev of
      Nothing -> putStrLn $ "Could not find device: " ++ devName
      Just x -> print =<< triggerScan bus x

doDisconnect :: String -> IO ()
doDisconnect devName = do
    bus <- connectSystem

    dev <- findDevice bus devName
    case dev of
      Nothing -> putStrLn $ "Could not find device: " ++ devName
      Just x -> print =<< triggerDisconnect bus x

doConnect :: String -> String -> IO ()
doConnect devName netName = do
    bus <- connectSystem
    registerAgent bus $ Agent agentRP agentC agentR
    dev <- findDevice bus devName

    case dev of
        Nothing -> putStrLn $ "Couldn't find device: " ++ devName
        Just x -> do
            nets <- getNetworks bus x
            let net = find ((==) netName . networkName) nets
            case net of
                Nothing -> putStrLn $ "Couldn't find the network: " ++ netName
                Just y -> print =<< triggerConnect bus y

main :: IO ()
main = do
    args <- getArgs
    case args of
      [] -> printDevices
      ("--scan":devices) -> mapM_ doScan devices
      ("--disconnect":devices) -> mapM_ doDisconnect devices
      ["--connect",device,network] -> doConnect device network
      _ -> putStrLn "Chocked on commandline :("

agentR :: IO ()
agentR = return ()

agentRP :: ObjectPath -> IO (Maybe String)
agentRP path = do
    putStr "Being asked for passphrase for: "
    print path
    return $ Just "NetworkCoding2016"

agentC :: String -> IO ()
agentC reason = putStrLn reason
