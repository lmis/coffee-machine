{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
module Cook
  ( cookRecipe
  , makeBeverage
  )
where

import           BeverageConfig                 ( BeverageConfig(..) )
import           Recipe                         ( Recipe(..)
                                                , RecipeStep(..)
                                                , RecipeType(..)
                                                , RecipeStatus(..)
                                                , RecipeInstruction(..)
                                                )
import           Service                        ( Service
                                                , ServiceState(..)
                                                , Task(..)
                                                , modifyServiceState
                                                , readServiceState
                                                )
import           Driver                         ( Driver
                                                , DriverReadings(..)
                                                , BrewerState(..)
                                                , readings
                                                , grinder
                                                , boilerOutValve
                                                , pump
                                                , brewerOutValve
                                                , sendCommands
                                                , brewerState
                                                , brewerPosToState
                                                )
import           Control.Monad                  ( unless
                                                , void
                                                )
import           Control.Concurrent             ( killThread
                                                , threadDelay
                                                , forkIO
                                                )
import           Control.Exception              ( finally
                                                , onException
                                                )
import           Data.List                      ( nub )
import           Data.IORef                     ( IORef
                                                , newIORef
                                                , readIORef
                                                , modifyIORef
                                                , atomicModifyIORef
                                                )




initSteps :: [RecipeInstruction] -> [RecipeStep]
initSteps = fmap (\(index, instruction) -> RecipeStep { status = Pending, .. }) . zip [0 ..]

-- TODO: Lenses?
modifyStepStatus :: RecipeStatus -> Integer -> Service -> IO ()
modifyStepStatus status' i service = modifyServiceState service $ \srv@ServiceState { currentTask } ->
  case currentTask of
    Nothing -> srv
    Just t ->
      let r = recipe t
          s = steps r
      in  srv { currentTask = Just t { recipe = r { steps = setStatus s } } }
      where setStatus = fmap (\s -> if index s == i then s { status = status' } else s)

makeBeverage :: BeverageConfig -> Service -> IO ()
makeBeverage BeverageConfig { instructions } service =
  cookRecipe service $ Recipe { recipeType = Beverage, steps = initSteps instructions }

cookRecipe :: Service -> Recipe -> IO ()
cookRecipe service recipe@Recipe { steps } = do
  d                   <- driver <$> readServiceState service
  cleanupInstructions <- newIORef []
  let run = void . sequence $ performStep d cleanupInstructions <$> steps
  let
    clean = do
      cleanup <- nub . initSteps <$> readIORef cleanupInstructions
      modifyServiceState service
        $ \srv -> srv
            { currentTask = Just Task { recipe = Recipe { recipeType = Cleanup, steps = cleanup }, kill = return () }
            }
      void . parallel $ performStep d cleanupInstructions <$> cleanup
      modifyServiceState service $ \srv -> srv { currentTask = Nothing }

  threadId <- forkIO $ finally run clean
  modifyServiceState service $ \srv -> srv { currentTask = Just Task { kill = killThread threadId, .. } }
 where
  performStep :: Driver -> IORef [RecipeInstruction] -> RecipeStep -> IO ()
  performStep d cleanup RecipeStep { index, instruction } = do
    onException
        (do
          modifyStepStatus Doing index service
          executeStep d cleanup instruction
          modifyStepStatus Done index service
        )
      $ modifyStepStatus Aborted index service
  executeStep :: Driver -> IORef [RecipeInstruction] -> RecipeInstruction -> IO ()
  executeStep _ cleanup instruction@DesiredTemperature { target } = do
    before <- desiredTemperature <$> readServiceState service
    modifyIORef cleanup (DesiredTemperature before :)
    modifyServiceState service $ \srv -> srv { desiredTemperature = target }
    modifyIORef cleanup $ filter (/= instruction)
  executeStep d cleanup Grind { grams } = do
    moveBrewer Receive service
    modifyIORef cleanup (DropGrounds :)
    finally
      (do
        sendCommands d $ \c -> c { grinder = True }
        threadDelay . round $ grams * 200000
      )
      (sendCommands d $ \c -> c { grinder = False })
  executeStep d cleanup DropGrounds = do
    moveBrewer Release service
    modifyIORef cleanup $ filter (/= DropGrounds)
    sendCommands d $ \c -> c { brewerState = Receive }
  executeStep d _ Extract { milliliters } = do
    moveBrewer Press service
    waitForTemperature service
    finally
      (do
        sendCommands d $ \c -> c { boilerOutValve = True, brewerOutValve = True, pump = True }
        threadDelay . round $ milliliters * 100000
      )
      (sendCommands d $ \c -> c { boilerOutValve = False, brewerOutValve = False, pump = False })
    modifyServiceState service $ \srv -> srv { cupFilled = True }

waitForTemperature :: Service -> IO ()
waitForTemperature service = waitForIt $ do
  serviceState <- readServiceState service
  let r = readings . driverState $ serviceState
  return $ (desiredTemperature serviceState - waterTemperature r) < 5

moveBrewer :: BrewerState -> Service -> IO ()
moveBrewer desired service = do
  s <- readServiceState service
  let d = driver s
  sendCommands d $ \c -> c { brewerState = desired }
  waitForIt $ do
    current <- brewerPosToState . brewerPosition . readings . driverState <$> readServiceState service
    return $ current == desired

waitForIt :: IO Bool -> IO ()
waitForIt test = do
  res <- test
  unless res $ do
    threadDelay 20000
    waitForIt test

parallel :: [IO ()] -> IO ()
parallel []       = return ()
parallel [x     ] = x
parallel (x : xs) = do
  count <- newIORef $ length xs
  _     <- forkIO $ parallel' count xs
  _     <- x
  waitForIt $ (== 0) <$> readIORef count
 where
  parallel' :: IORef Int -> [IO ()] -> IO ()
  parallel' _   []       = return ()
  parallel' ref (y : ys) = do
    _ <- forkIO $ parallel' ref ys
    _ <- y
    atomicModifyIORef ref (\a -> (a - 1, ()))

