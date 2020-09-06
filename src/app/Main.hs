{-# LANGUAGE RecordWildCards, NamedFieldPuns#-}
module Main
  ( main
  )
where

import           Control.Monad.IO.Class         ( liftIO )
import           DriverSim                      ( runDriverSimulation )
import           Driver                         ( Driver
                                                , DriverCommands(..)
                                                , DriverState(..)
                                                , brewerOutValve
                                                , sendReadings
                                                , DriverReadings(..)
                                                )
import           Service                        ( Service
                                                , ServiceState(..)
                                                , readServiceState
                                                , runService
                                                , abort
                                                )
import           BeverageConfig                 ( beverages )
import           Cook                           ( makeBeverage )
import           Widgets                        ( displayWidget
                                                , machineWidget
                                                , debugWidget
                                                , Name(..)
                                                )
import           Control.Monad                  ( forever
                                                , void
                                                )
import           Control.Concurrent             ( ThreadId
                                                , threadDelay
                                                , forkIO
                                                )
import           Brick                          ( App(..)
                                                , AttrMap
                                                , BrickEvent(..)
                                                , EventM
                                                , Next
                                                , Widget
                                                -- , Padding(..)
                                                , customMain
                                                , neverShowCursor
                                                , continue
                                                , halt
                                                , attrMap
                                                , vBox
                                                )
import qualified Brick.Widgets.Center          as C
import qualified Graphics.Vty                  as V
import           Brick.BChan                    ( BChan
                                                , newBChan
                                                , writeBChan
                                                )

data AppEvent = UiResample
data AppState = AppState {
  service :: Service,
  serviceState :: ServiceState,
  driverSim :: Driver,
  pouringIndex :: Int
}
app :: App AppState AppEvent Name
app = App { appDraw         = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent  = handleEvent
          , appStartEvent   = return
          , appAttrMap      = const theMap
          }

main :: IO ()
main = do
  driverSim    <- runDriverSimulation
  service      <- runService driverSim
  serviceState <- readServiceState service

  -- channel to inject events into main loop
  chan         <- newBChan 10
  _            <- uiResample chan

  initialVty   <- buildVty
  void $ customMain initialVty buildVty (Just chan) app $ AppState { pouringIndex = 0, .. }
 where
  buildVty = do
    vty <- V.mkVty V.defaultConfig
    liftIO $ V.setMode (V.outputIface vty) V.Mouse True
    return vty

uiResample :: BChan AppEvent -> IO ThreadId
uiResample chan = forkIO $ forever $ do
  writeBChan chan UiResample
  threadDelay 100000

serviceCommand :: AppState -> (Service -> IO ()) -> EventM Name (Next AppState)
serviceCommand s@AppState { service } command = do
  _ <- liftIO $ command service
  continue s

handleEvent :: AppState -> BrickEvent Name AppEvent -> EventM Name (Next AppState)
handleEvent s@AppState { service, pouringIndex } (AppEvent UiResample) = do
  serviceState' <- liftIO $ readServiceState service
  continue $ s { pouringIndex = pouringIndex' . commands $ driverState serviceState', serviceState = serviceState' }
 where
  pouringIndex' DriverCommands { brewerOutValve } | pouringIndex < 6 && brewerOutValve  = pouringIndex + 1
                                                  | pouringIndex < 6                    = pouringIndex
                                                  | pouringIndex == 6 && brewerOutValve = 6
                                                  | otherwise                           = (pouringIndex + 1) `mod` 12
handleEvent s (VtyEvent (V.EvKey (V.KChar 'c') [])) = serviceCommand s $ makeBeverage (head beverages)
handleEvent s (MouseDown (Cook config) V.BLeft _ _) = serviceCommand s $ makeBeverage config
handleEvent s (MouseDown Abort         V.BLeft _ _) = serviceCommand s abort
handleEvent s (VtyEvent (V.EvKey V.KEsc [])       ) = do
  serviceCommand s abort
handleEvent s@AppState { driverSim } (VtyEvent (V.EvKey (V.KChar 'p') [])) = do
  _ <- liftIO $ sendReadings driverSim $ \r@DriverReadings { cupDetected } -> r { cupDetected = not cupDetected }
  continue s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) = halt s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'd') [V.MCtrl])) = halt s
handleEvent s _ = continue s

drawUI :: AppState -> [Widget Name]
drawUI AppState { serviceState, pouringIndex } =
  [ C.vCenter $ vBox
      (C.hCenter <$> [displayWidget serviceState, machineWidget pouringIndex serviceState, debugWidget serviceState])
  ]

theMap :: AttrMap
theMap = attrMap V.defAttr []

-- TODO: Make Store state. Contains more info than driver such as isPouring / pouringProgress
