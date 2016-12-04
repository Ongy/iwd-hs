{-# LANGUAGE OverloadedStrings #-}
module Network.IWD.Agent
    ( Agent (..)
    , registerAgent
    )
where

import Data.Maybe (mapMaybe)
import DBus
import DBus.Client

data Agent = Agent
    { agentRequest :: ObjectPath -> IO (Maybe String)
    , agentCancel :: String -> IO ()
    , agentRelease :: IO ()
    }


handleRequest :: (ObjectPath -> IO (Maybe String)) -> MethodCall -> IO Reply
handleRequest fun mCall = do
    let [path] = mapMaybe fromVariant . methodCallBody $ mCall
    ret <- fun path
    pure $ case ret of
        Nothing -> replyError "net.connman.iwd.Agent.Error.Canceled" []
        Just x -> replyReturn [toVariant x]

registerAgent :: Client -> Agent -> IO Bool
registerAgent bus agent = do
    export bus "/agent"
        [ method "net.connman.iwd.Agent" "RequestPassphrase" (signature_ [TypeObjectPath]) (signature_ [TypeString]) (handleRequest $ agentRequest agent)
        , autoMethod "net.connman.iwd.Agent" "Cancel" $ agentCancel agent
        , autoMethod "net.connman.iwd.Agent" "Release" $ agentRelease agent
        ]

    let path = "/"
    let name = "net.connman.iwd.AgentManager"
    let memb = "RegisterAgent"
    ret <- call bus (methodCall path name memb)
        { methodCallDestination = Just "net.connman.iwd"
        , methodCallBody = [toVariant ("/agent" :: ObjectPath)]
        }

    return $ case ret of
      Left _ -> False
      Right _ -> True
