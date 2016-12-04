{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Network.IWD.Query
    ( getNetwork
    , getNetworks
    , getDevice
    , getDevices
    , findDevice
    , triggerScan
    , triggerDisconnect
    , triggerConnect
    )
where

import Data.List (find)
import qualified Data.Map as M
import Data.Maybe (mapMaybe, fromMaybe)
import Network.IWD.Types

import DBus
import DBus.Client

getPropertyM :: IsVariant a => Client -> ObjectPath -> InterfaceName -> MemberName -> IO (Maybe a)
getPropertyM bus path iname mname = do
    let args =  [ toVariant . formatInterfaceName $ iname
                , toVariant . formatMemberName $ mname
                ]
    ret <- call bus (methodCall path "org.freedesktop.DBus.Properties" "Get") { methodCallDestination = Just "net.connman.iwd", methodCallBody = args }

    return $ case ret of
        Left _ -> Nothing
        Right z -> let x :: Variant = fromMaybe (error "meh") . fromVariant . head . methodReturnBody $ z
                    in fromVariant x

getProperty :: IsVariant a => Client -> ObjectPath -> InterfaceName -> MemberName -> IO a
getProperty bus path iname mname =
    let get = fromMaybe (error $ "Couldn't get property: " ++ show path ++ ',':show iname ++ ',':show mname)
     in get `fmap` getPropertyM bus path iname mname

getNetwork :: Client -> ObjectPath -> IO Network
getNetwork bus path =
    let get = getProperty bus path "net.connman.iwd.Network"
     in Network
            <$> return path
            -- GHC refuses to make get :: IsVaraint a => MemberName -> IO a :(
            <*> getProperty bus path "net.connman.iwd.Network" "Device"
            <*> get "Name"
            <*> return 0
            <*> get "Type"

getDevice :: Client -> ObjectPath -> IO Device
getDevice bus path =
    let getB = getProperty bus path "net.connman.iwd.Device"
        getO = getProperty bus path "net.connman.iwd.Device"
        getS = getProperty bus path "net.connman.iwd.Device"
     in Device
            <$> pure path
            <*> getB "Powered"
            <*> getB "Connected"
            <*> getO "Adapter"
            <*> getPropertyM bus path "net.connman.iwd.Device" "ConnectedNetwork"
            <*> getS "Address"
            <*> getS "Name"
            <*> getS "State"

-- | Get all the networks available on the Device.
getNetworks :: Client -> Device -> IO [Network]
getNetworks bus dev = do
    let path = devicePath dev
    let name = "net.connman.iwd.Device"
    let memb = "GetOrderedNetworks"

    reply <- call_ bus (methodCall path name memb) { methodCallDestination = Just "net.connman.iwd" }
    let nets = methodReturnBody reply

    return . concat . mapMaybe (parseNetworks path) $ nets

-- | List all devices iwd manages.
getDevices :: Client -> IO [Device]
getDevices bus = do
    let path = "/"
    let name = "org.freedesktop.DBus.ObjectManager"
    let memb = "GetManagedObjects"

    reply <- call_ bus (methodCall path name memb) { methodCallDestination = Just "net.connman.iwd" }

    let alls = map parseObjectsFromManager $ methodReturnBody reply
    return . mapMaybe getIWDevice . concatMap M.elems $ alls

findDevice :: Client -> String -> IO (Maybe Device)
findDevice bus devName =
    find ((==) devName . deviceName) <$> getDevices bus

triggerScan :: Client -> Device -> IO MethodReturn
triggerScan bus dev = do
    let path = devicePath dev
    let name = "net.connman.iwd.Device"
    let memb = "Scan"

    call_ bus (methodCall path name memb) { methodCallDestination = Just "net.connman.iwd" }

triggerDisconnect :: Client -> Device -> IO MethodReturn
triggerDisconnect bus dev = do
    let path = devicePath dev
    let name = "net.connman.iwd.Device"
    let memb = "Disconnect"

    call_ bus (methodCall path name memb) { methodCallDestination = Just "net.connman.iwd" }

triggerConnect :: Client -> Network -> IO MethodReturn
triggerConnect bus net = do
    let path = networkPath net
    let name = "net.connman.iwd.Network"
    let memb = "Connect"

    call_ bus (methodCall path name memb) { methodCallDestination = Just "net.connman.iwd" }
