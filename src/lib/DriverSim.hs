{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
module DriverSim
  ( runDriverSimulation
  )
where

import           Driver                         ( Driver
                                                , DriverCommands(..)
                                                , DriverState(..)
                                                , boilerHeater
                                                , grinder
                                                , boilerOutValve
                                                , brewerOutValve
                                                , defaultCommands
                                                , DriverReadings(..)
                                                , brewerStateToPos
                                                )
import           Data.IORef                     ( newIORef
                                                , modifyIORef
                                                )
import           Control.Concurrent             ( threadDelay
                                                , forkIO
                                                )
import           Control.Monad                  ( forever )

runDriverSimulation :: IO Driver
runDriverSimulation = do
  ref <- newIORef DriverState
    { readings = DriverReadings { waterTemperature = 55
                                , waterLevel       = 100
                                , beanLevel        = 100
                                , cupDetected      = False
                                , spillage         = 0
                                , brewerPosition   = 50
                                }
    , commands = defaultCommands
    }
  _ <- forkIO $ forever $ do
    _ <- modifyIORef ref simulateTimeStep
    threadDelay 50000
  pure ref

simulateTimeStep :: DriverState -> DriverState
simulateTimeStep state@DriverState { readings, commands } = state { readings = update readings }
 where
  update reading@DriverReadings {..} = reading
    { waterTemperature = clampBoilerTemp
                           $ if boilerHeater commands then waterTemperature + 0.5 else waterTemperature - 0.03125
    , waterLevel       = clampWaterLevel $ (boilerOutWaterLevel . pumpWaterLevel) waterLevel
    , beanLevel        = max 0 $ if grinder commands then beanLevel - 0.125 else beanLevel
    , spillage         = if brewerOutValve commands && not cupDetected then spillage + 0.5 else spillage
    , brewerPosition   = case fuzzyCompare brewerPosition (brewerStateToPos $ brewerState commands) of
                           LT -> brewerPosition + 0.5
                           EQ -> brewerPosition
                           GT -> brewerPosition - 0.5
    }
  clampBoilerTemp x = max 25 $ min 100 x
  boilerOutWaterLevel x = if boilerOutValve commands then x - 0.325 else x
  pumpWaterLevel x = if pump commands then x + 0.25 else x
  clampWaterLevel x = max 0 $ min 100 x
  fuzzyCompare x y | x - y > 0.4  = GT
                   | x - y < -0.4 = LT
                   | otherwise    = EQ
