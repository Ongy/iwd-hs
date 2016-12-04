{-# LANGUAGE OverloadedStrings #-}
module State
where

import GHC.Exts (IsList(fromList))
import DBus
import DBus.Client
import Network.IWD.Types

import Brick.Widgets.List
import Brick.Widgets.Edit
import Brick.Widgets.Core (vBox, str)

import Zipper

type RName = ObjectPath
type LType = List RName Network
type DeviceList = Zipper (Device, LType)
-- TODO: Use lenses and make this less shitty
type PassQuery = ((Network, Maybe String -> IO()), Editor String RName)

data State = State
    { -- | The dbus connection
      stateBus :: Client
      -- | All currently connected devices
    , stateDevices :: Maybe DeviceList
      -- | Network to ask passphrase for. If Just
    , stateQuery :: Maybe PassQuery
    }

newState :: Client -> State
newState bus = State
    { stateBus = bus
    , stateDevices = Nothing
    , stateQuery = Nothing
    }

updateDevs :: (DeviceList -> DeviceList) -> State -> State
updateDevs _ x@(State _ Nothing _) = x
updateDevs f (State bus (Just devs) query) = State bus (Just $ f devs) query

updateDevsA :: Applicative f => (DeviceList -> f DeviceList) -> State -> f State
updateDevsA _ x@(State _ Nothing _) = pure x
updateDevsA f (State bus (Just devs) query) =
    (\x -> State bus (Just x) query) `fmap` f devs

updateQuery :: (PassQuery -> PassQuery) -> State -> State
updateQuery _ s@(State _ _ Nothing) = s
updateQuery f s@(State _ _ (Just x)) = s { stateQuery = Just (f x) }

updateQueryA :: Applicative f => (PassQuery -> f PassQuery) -> State -> f State
updateQueryA _ s@(State _ _ Nothing) = pure s
updateQueryA f s@(State _ _ (Just x)) =
    (\v -> s { stateQuery = Just v }) `fmap` f x

getSelectedNet :: State -> Maybe Network
getSelectedNet s = 
    appF (fmap snd . listSelectedElement . snd) =<< stateDevices s

setDevices :: State -> [(Device, LType)] -> State
setDevices (State bus _ query) [] = State bus Nothing query
setDevices (State bus _ query) ndevs =
    State bus (Just $ fromList ndevs) query


queryNet :: Network -> (Maybe String -> IO ()) -> State -> State
queryNet net f s =
    let edit = editor (networkPath net) (vBox . map str) (Just 1) ""
     in s { stateQuery = Just ((net, f), edit) }

cancelQuery :: State -> State
cancelQuery s = s { stateQuery = Nothing }

addDevice :: (Device, LType) -> State -> State
addDevice x s@(State _ Nothing _) =
    s { stateDevices = Just $ fromList [x] }
addDevice x s@(State _ (Just xs) _) =
    s { stateDevices = Just . app x $ xs }
