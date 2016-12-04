{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main
where

import System.IO (hPutStrLn, stderr)
import Zipper
import GHC.Exts (IsList(toList))

import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar, newMVar)
import Control.Monad (void)
import Control.Applicative ((<$>))
import Control.Concurrent (Chan, newChan, writeChan, forkIO)
import Control.Monad.IO.Class
import DBus
import DBus.Client
import Data.Default
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe, listToMaybe)
import Data.Vector (fromList)
import Network.IWD.Query
import Network.IWD.Types
import Network.IWD.Agent
import State

import Brick
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Edit as E
import qualified Brick.AttrMap as A
import Brick.Util (bg)

import Graphics.Vty.Attributes -- (green, blue)
import qualified Graphics.Vty as V

newtype TuiSignalHandler = TuiSignalHandler (MVar (Map ObjectPath SignalHandler))

data TuiEvent
    -- We want to start and ask for the password
    = AskPass Network (Maybe String -> IO ())
    -- Cancel the password questioning
    | CancelPass String
    -- A new object apeared, add it (we will rebuild for now)
    | NewDevice (Device,LType)
    -- The state of our device changed.
    | DevStateChanged ObjectPath String
    | DevNetChanged ObjectPath (Maybe ObjectPath)

updateConnected :: ObjectPath -> Maybe ObjectPath -> DeviceList -> DeviceList
updateConnected devPath net =
    fmap (\old@(dev, nets) -> if devicePath dev == devPath
            then (dev { deviceConNetwork = net }, nets)
            else old)

-- This is [] to encode none as Nil
askPassphrase :: Maybe PassQuery -> [Widget RName]
askPassphrase Nothing = []
askPassphrase (Just ((net, _), edit)) = return . C.vCenter . C.hCenter . vBox $
    [ (str $ "Please enter the passord for: " ++ networkName net)
    , E.renderEditor True edit
    ]

renderNetList :: Maybe ObjectPath -> Bool -> Network -> Widget RName
renderNetList con l net =
    let attr = if Just (networkPath net) == con
        then withAttr $ attrName "connectedNet"
        else id
        focus = if l then (++ " <-") else id
     in attr . str . focus . networkName $ net

renderDevice :: Bool -> (Device, L.List RName Network) -> Widget RName
renderDevice f (dev, nets) =
    let attr = if f then withAttr (attrName "focusedDev") else id
     in attr . B.border $ vBox
        [ C.hCenter . str $ deviceName dev ++ "  (" ++ deviceState dev ++ ")"
        , L.renderList (renderNetList $ deviceConNetwork dev) f nets
        ]

renderState :: State -> [Widget RName]
renderState s =
    let pass = askPassphrase $ stateQuery s
        devs = toList . mapF renderDevice <$> stateDevices s
     in pass ++ [C.vCenter . C.hCenter . vBox $ fromMaybe [] devs]

disconnectDevice :: MonadIO m => State -> m ()
disconnectDevice s = case stateDevices s of
    Nothing -> return ()
    Just x -> liftIO $ appF (void . triggerDisconnect (stateBus s) . fst) x

handleEvent :: State -> BrickEvent RName TuiEvent -> EventM RName (Next State)
-- Catch events while we have a password question open.
handleEvent s@(State _ _ (Just ((_, f), edit))) (VtyEvent e) = case e of
    V.EvKey V.KEnter [] -> do
        liftIO $ f . listToMaybe . E.getEditContents $ edit
        continue s { stateQuery = Nothing }
    V.EvKey V.KEsc [] -> continue $ cancelQuery s
    _ -> continue =<< updateQueryA (traverse (E.handleEditorEvent e)) s
handleEvent s (VtyEvent e) = case e of
    V.EvKey (V.KChar 'q') [] -> halt s
    V.EvKey (V.KChar '\t') [] -> continue $ updateDevs advance s
    V.EvKey (V.KBackTab) [] -> continue $ updateDevs goBack s
    V.EvKey (V.KChar 'd') [] -> disconnectDevice s >> continue s
    V.EvKey V.KEnter [] -> do
        case getSelectedNet s of
            Nothing -> return ()
            Just x -> liftIO $ void . forkIO . void . triggerConnect (stateBus s) $ x
        continue s
    _ -> continue =<< updateDevsA (modifyF . traverse $ L.handleListEvent e) s
handleEvent s (AppEvent (AskPass net fun)) = do
    liftIO $ putStrLn $ "Starting query for net: " ++ show net
    continue $ queryNet net fun s
handleEvent s (AppEvent (CancelPass _)) = continue $ cancelQuery s
handleEvent s (AppEvent (NewDevice dev)) =
    continue $ addDevice dev s
handleEvent s (AppEvent (DevStateChanged devPath new)) =
    let fun = (\old@(dev, xs) -> if devPath == devicePath dev then (dev { deviceState = new }, xs) else old)
     in continue $ updateDevs (fmap fun) s
handleEvent s (AppEvent (DevNetChanged devPath new)) =
    continue $ updateDevs (updateConnected devPath new) s
handleEvent s _ = continue s

getInitialNetworks :: Client -> Device -> IO (Device, LType)
getInitialNetworks bus dev = do
    nets <- getNetworks bus dev
    return (dev, L.list (devicePath dev) (fromList nets) 1)

getInitialDevices :: Client -> IO [(Device, LType)]
getInitialDevices bus = do
    devices <- getDevices bus
    mapM (getInitialNetworks bus) devices

theMap :: Bool -> AttrMap
theMap c = A.applyAttrMappings attrs def
    where attrs =   [ ("connectedNet", Attr KeepCurrent (SetTo green) KeepCurrent)--fg green)
                    , ("focusedDev", bg $ if c then blue else red)
                    ]

tuiApp :: Bool -> App State TuiEvent RName
tuiApp c = App
    { appDraw = renderState
    , appChooseCursor = neverShowCursor
    , appHandleEvent = handleEvent
    , appStartEvent = return
    , appAttrMap = const (theMap c)
    }


handleRequest :: Client -> Chan TuiEvent -> ObjectPath -> IO (Maybe String)
handleRequest bus chan obj = do
    net <- getNetwork bus obj
    var <- newEmptyMVar
    writeChan chan $ AskPass net (putMVar var)
    takeMVar var

handleDSSignal :: Client -> Chan TuiEvent -> Signal -> IO ()
handleDSSignal _ chan sig = do
    -- when this is called we are guaranteed:
    -- PropertiesChanged on a net.connman.iwd.Device
    let [_, v, i] = signalBody $ sig

    let body :: Map String Variant = fromJust . fromVariant $ v
    let inval :: [String] = fromJust . fromVariant $ i

    case fromVariant =<< M.lookup "State" body of
        Nothing -> pure ()
        Just x -> writeChan chan $ DevStateChanged (signalPath sig) x

    -- This is sent if we got a new value (connecting)
    case fromVariant =<< M.lookup "ConnectedNetwork" body of
        Nothing -> pure ()
        Just x -> writeChan chan $ DevNetChanged (signalPath sig) (Just x)
    -- This is sent if we don't have a new value (disconnect)
    if "ConnectedNetwork" `elem` inval
       then writeChan chan $ DevNetChanged (signalPath sig) Nothing
       else pure ()


-- newtype TuiSignalHandler = TuiSignalHandler (MVar (Map ObjectPath SignalHandler))
handleNewDevice :: Client -> Chan TuiEvent -> TuiSignalHandler -> Device -> IO ()
handleNewDevice bus chan (TuiSignalHandler ref) dev = do
    sMap <- takeMVar ref

    let handler = handleDSSignal bus chan
    let iface =  Just "org.freedesktop.DBus.Properties"
    let member = Just "PropertiesChanged"
    let path = Just $ devicePath dev
    let match = matchAny
                { matchPath = path
                , matchInterface = iface
                , matchMember = member
                }

    handle <- addMatch bus match handler

    val <- getInitialNetworks bus dev
    putMVar ref $ M.insert (devicePath dev) handle sMap
    hPutStrLn stderr $ "Handing \"" ++ deviceName dev ++ "\" to brick"
    writeChan chan $ NewDevice val

handleNewSignal :: Client -> Chan TuiEvent -> TuiSignalHandler -> Signal -> IO ()
handleNewSignal = undefined

setupSignals :: Client -> Chan TuiEvent -> IO ()
setupSignals bus chan = do
    state <- TuiSignalHandler <$> newMVar mempty

--     let handler = handleNewSignal bus chan state
--     let iface =  Just "org.Freedesktop.DBus.ObjectManager"
--     let member = Just "InterfacesAdded"
--     let match = matchAny
--                 { matchPath = Just "/"
--                 , matchInterface = iface
--                 , matchMember = member
--                 }
-- 
--     _ <- addMatch bus match handler
-- 
    mapM_ (handleNewDevice bus chan state) =<< getDevices bus


main :: IO ()
main = do
    bus <- connectSystem
    chan <- newChan

    c <- registerAgent bus $ Agent
        { agentRequest = handleRequest bus chan
        , agentCancel = writeChan chan . CancelPass
        , agentRelease = return ()
        }

    setupSignals bus chan

    void . customMain (V.mkVty def) (Just chan) (tuiApp c) $ newState bus
