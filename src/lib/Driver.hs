{-# LANGUAGE NamedFieldPuns #-}
module Driver
  ( Driver
  , DriverState(..)
  , DriverReadings(..)
  , DriverCommands(..)
  , BrewerState(..)
  , defaultCommands
  , sendCommands
  , sendReadings
  , readDriverState
  , brewerPosToState
  , brewerStateToPos
  )
where

import           Data.IORef                     ( IORef
                                                , modifyIORef
                                                , readIORef
                                                )

data BrewerState = Press
                    | Close
                    | Receive
                    | Release
                    | Pos Double
                    deriving (Show, Read, Eq, Ord)

data DriverCommands = DriverCommands {
  boilerHeater :: Bool,
  grinder :: Bool,
  brewerState :: BrewerState,
  waterLowIndicator :: Bool,
  pump :: Bool,
  groundsContainerFullIndicator :: Bool,
  boilerOutValve :: Bool,
  brewerOutValve :: Bool
} deriving (Show, Read, Eq, Ord)

data DriverReadings = DriverReadings {
  waterTemperature :: Double,
  waterLevel :: Double,
  beanLevel :: Double,
  cupDetected :: Bool,
  spillage :: Double,
  brewerPosition :: Double
} deriving (Show, Read, Eq, Ord)

data DriverState = DriverState {
  readings :: DriverReadings,
  commands :: DriverCommands
} deriving (Show, Read, Eq, Ord)

type Driver = IORef DriverState
defaultCommands :: DriverCommands
defaultCommands = DriverCommands { boilerHeater                  = False
                                 , grinder                       = False
                                 , brewerState                   = Receive
                                 , waterLowIndicator             = False
                                 , pump                          = False
                                 , groundsContainerFullIndicator = False
                                 , boilerOutValve                = False
                                 , brewerOutValve                = False
                                 }

sendCommands :: Driver -> (DriverCommands -> DriverCommands) -> IO ()
sendCommands driver setter = modifyIORef driver $ \d@DriverState { commands } -> d { commands = setter commands }

sendReadings :: Driver -> (DriverReadings -> DriverReadings) -> IO ()
sendReadings driver setter = modifyIORef driver $ \d@DriverState { readings } -> d { readings = setter readings }

readDriverState :: Driver -> IO DriverState
readDriverState = readIORef

brewerPosToState :: Double -> BrewerState
brewerPosToState pos | pos =~= 95 = Press
                     | pos =~= 90 = Close
                     | pos =~= 80 = Receive
                     | pos =~= 0  = Release
                     | otherwise  = Pos pos
 where
  (=~=) :: Double -> Double -> Bool
  x =~= y = abs (x - y) < 0.5

brewerStateToPos :: BrewerState -> Double
brewerStateToPos Press   = 95
brewerStateToPos Close   = 90
brewerStateToPos Receive = 80
brewerStateToPos Release = 0
brewerStateToPos (Pos x) = x
