{-# LANGUAGE ScopedTypeVariables #-}
module Network.IWD.Types
    ( AgentManager
    , Adapter (..)
    , Device (..)
    , Network (..)
    , IWDObject (..)

    , getIWDevice
    , parseObjectsFromManager
    , parseNetworks
    )
where

import Data.Maybe (fromMaybe)
import Control.Monad (join)
import Control.Applicative ((<|>))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Int (Int16)

-- We need the dbus here for decoding.
import DBus (IsVariant(..), ObjectPath, Variant)

-- | Data type for the AgentManager. This is only for typesafety.
data AgentManager = AgentManager ObjectPath deriving (Eq, Show)

-- | A Adapter from iwd.
data Adapter = Adapter
    { adapterPath    :: ObjectPath
    , adapterPowered :: Bool
    , adapterModel   :: String
    , adapterVendor  :: String
    } deriving (Eq, Show)

-- | A device form iwd. This equals a device from ip link
data Device = Device
    { devicePath       :: ObjectPath
    , devicePowered    :: Bool
    , deviceScanning   :: Bool
    , deviceAdapter    :: ObjectPath
    , deviceConNetwork :: Maybe ObjectPath
    , deviceAddress    :: String
    , deviceName       :: String
    , deviceState      :: String
    } deriving (Eq, Show)

-- | A network from iwd. These are groups by ESSID.
data Network = Network
    { networkPath   :: ObjectPath
    , networkDevice :: ObjectPath
    , networkName   :: String
    , networkSignal :: Int
    , networkType   :: String
    } deriving (Eq, Show)

-- | Accumulator type, so we can parse ObjectManager $foo
data IWDObject
    = IWAdapter Adapter
    | IWDevice Device
    | IWNetwork Network
    | IWAgentManager AgentManager
    deriving (Eq, Show)


getIWDevice :: IWDObject -> Maybe Device
getIWDevice (IWDevice x) = Just x
getIWDevice _ = Nothing

parseAgentManagerFromManager :: ObjectPath -> Map String (Map String Variant) -> Maybe AgentManager
parseAgentManagerFromManager path x = const (AgentManager path) <$> M.lookup "net.connman.iwd.AgentManager" x

parseAdapterFromManager :: ObjectPath -> Map String (Map String Variant) -> Maybe Adapter
parseAdapterFromManager path m = do
    vals <- M.lookup "net.connman.iwd.Adapter" m
    model <- fromVariant =<< M.lookup "Model" vals
    vendor <- fromVariant =<< M.lookup "Vendor" vals
    powered <- fromVariant =<< M.lookup "Powered" vals
    return $ Adapter path powered model vendor

parseNetworkFromManager :: ObjectPath -> Map String (Map String Variant) -> Maybe Network
parseNetworkFromManager path m = do
    vals <- M.lookup "net.connman.iwd.Network" m
    device <- fromVariant =<< M.lookup "Device" vals
    ssid <- fromVariant =<< M.lookup "Name" vals
    -- signal <- fromVariant =<< M.lookup "Signal" vals
    enc <- fromVariant =<< M.lookup "Type" vals
    return $ Network path device ssid 0 enc

parseDeviceFromManager :: ObjectPath -> Map String (Map String Variant) -> Maybe Device
parseDeviceFromManager path m = do
    vals <- M.lookup "net.connman.iwd.Device" m
    adapter :: ObjectPath <- fromVariant =<< M.lookup "Adapter" vals
    address :: String <- fromVariant =<< M.lookup "Address" vals
    name :: String <- fromVariant =<< M.lookup "Name" vals
    let network = join . fmap fromVariant $ M.lookup "ConnectedNetwork" vals
    powered :: Bool <- fromVariant =<< M.lookup "Powered" vals
    scanning :: Bool <- fromVariant =<< M.lookup "Scanning" vals
    state :: String <- fromVariant =<< M.lookup "State" vals
    return $ Device path powered scanning adapter network address name state


parseObjectFromManager :: ObjectPath -> Map String (Map String Variant) -> IWDObject
parseObjectFromManager path x = fromMaybe (error $ "Couldn't decode Object from Manager: " ++ show x) $
    fmap IWAgentManager (parseAgentManagerFromManager path x)
    <|> fmap IWAdapter (parseAdapterFromManager path x)
    <|> fmap IWNetwork (parseNetworkFromManager path x)
    <|> fmap IWDevice (parseDeviceFromManager path x)
    <|> Nothing

parseObjectsFromManager :: Variant -> Map ObjectPath IWDObject
parseObjectsFromManager x = 
    let Just y = fromVariant x
     in M.mapWithKey parseObjectFromManager y

parseNetwork :: ObjectPath -> (ObjectPath, String, Int16, String) -> Network
parseNetwork device (path, ssid, signal, enc) = Network
    { networkPath = path
    , networkDevice = device
    , networkName = ssid
    , networkSignal = fromIntegral signal
    , networkType = enc
    }

parseNetworks :: ObjectPath -> Variant -> Maybe [Network]
parseNetworks path = fmap (map $ parseNetwork path) . fromVariant
