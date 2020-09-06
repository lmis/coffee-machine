{-# LANGUAGE RecordWildCards, NamedFieldPuns#-}
module Service
  ( Service
  , ServiceState(..)
  , Task(..)
  , readServiceState
  , modifyServiceState
  , runService
  , abort
  )
where

import           Driver                         ( Driver
                                                , DriverCommands(..)
                                                , DriverState
                                                , DriverReadings(..)
                                                , readings
                                                , cupDetected
                                                , boilerHeater
                                                , pump
                                                , sendCommands
                                                , readDriverState
                                                )
import           Recipe                         ( Recipe(..)
                                                , RecipeType(..)
                                                )
import           Control.Monad                  ( forever
                                                , unless
                                                )
import           Control.Monad.STM              ( atomically )
import           Control.Concurrent.STM.TVar    ( TVar
                                                , newTVar
                                                , modifyTVar
                                                , readTVarIO
                                                )
import           Control.Concurrent             ( threadDelay
                                                , forkIO
                                                )

data Task = Task {
  kill :: IO (),
  recipe :: Recipe

}

data ServiceState = ServiceState {
  driver :: Driver,
  driverState :: DriverState,
  cupFilled :: Bool,
  desiredTemperature :: Double,
  currentTask :: Maybe Task
}

type Service = TVar ServiceState
type CommandUpdate = DriverCommands -> DriverCommands
type ServiceUpdate = ServiceState -> ServiceState

runService :: Driver -> IO Service
runService driver = do
  driverState <- readDriverState driver
  service <- atomically $ newTVar ServiceState { cupFilled = False, currentTask = Nothing, desiredTemperature = 70, .. }

  _ <- forkIO . forever $ do
    readDriver driver service
    threadDelay 20000

  return service

readDriver :: Driver -> Service -> IO ()
readDriver driver service = do
  serviceState <- readServiceState service
  driverState' <- readDriverState driver
  modifyServiceState service $ \srv -> srv { driverState = driverState' }

  let newReadings = readings driverState'
  modifyServiceState service $ resetCupIfNeeded newReadings
  sendCommands driver
    $ regulateWaterLevel newReadings
    . regulateTemperature (desiredTemperature serviceState) newReadings
 where
  resetCupIfNeeded :: DriverReadings -> ServiceUpdate
  resetCupIfNeeded newReadings srv | cupDetected newReadings = srv
                                   | otherwise               = srv { cupFilled = False }
  regulateWaterLevel :: DriverReadings -> CommandUpdate
  regulateWaterLevel DriverReadings { waterLevel } c = c { pump = waterLevel < 98 }
  regulateTemperature :: Double -> DriverReadings -> CommandUpdate
  regulateTemperature target DriverReadings { waterTemperature, waterLevel } c
    | waterTemperature > target || waterLevel < 10 = c { boilerHeater = False }
    | waterTemperature < target - 3 = c { boilerHeater = True }
    | otherwise                     = c

abort :: Service -> IO ()
abort service = abort' =<< readServiceState service
 where
  abort' ServiceState { currentTask } = case currentTask of
    Just task -> unless ((== Cleanup) . recipeType . recipe $ task) $ do
      _ <- kill task
      modifyServiceState service $ \srv -> srv { currentTask = Nothing }
    Nothing -> return ()

readServiceState :: Service -> IO ServiceState
readServiceState = readTVarIO

modifyServiceState :: Service -> (ServiceState -> ServiceState) -> IO ()
modifyServiceState service setter = atomically $ modifyTVar service setter
