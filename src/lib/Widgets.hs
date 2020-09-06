{-# LANGUAGE RecordWildCards, NamedFieldPuns#-}
module Widgets
  ( Name(..)
  , displayWidget
  , machineWidget
  , debugWidget
  )
where

import           BeverageConfig                 ( BeverageConfig(..)
                                                , beverages
                                                )
import           Recipe                         ( Recipe(..)
                                                , RecipeStep(..)
                                                , RecipeType(..)
                                                )
import           Driver                         ( DriverState(..)
                                                , DriverReadings(..)
                                                , DriverCommands(..)
                                                , brewerPosToState
                                                )
import           Service                        ( ServiceState(..)
                                                , Task(..)
                                                )
import           Sprites
import           Brick                          ( Widget
                                                , Padding(..)
                                                , padLeft
                                                , padRight
                                                , padTop
                                                , padBottom
                                                , padLeftRight
                                                , withBorderStyle
                                                , clickable
                                                , vBox
                                                , str
                                                , (<+>)
                                                )
import qualified Brick.Widgets.Border          as B
import qualified Brick.Widgets.Border.Style    as BS
import qualified Brick.Widgets.Center          as C

import           Data.Maybe                     ( isJust
                                                , fromJust
                                                )

data Name = Cook BeverageConfig | Abort deriving (Show, Eq, Ord)

displayWidget :: ServiceState -> Widget Name
displayWidget state = labeledVBox "Display" $ C.hCenter <$> vendingWidgets state

vendingWidgets :: ServiceState -> [Widget Name]
vendingWidgets ServiceState { cupFilled, currentTask }
  | isJust currentTask = case recipeType . recipe $ fromJust currentTask of
    Beverage -> [abort, str "Making your beverage..."]
    Cleanup ->
      if cupFilled then [str "Enjoy your beverage while I cleanup!"] else [str "Cleaning up... Just a moment."]
  | cupFilled = [str "Enjoy your beverage!"]
  | otherwise = fmap (\b -> C.hCenter $ button (name b) (Cook b)) beverages
  where abort = button "X" Abort

machineWidget :: Int -> ServiceState -> Widget a
machineWidget pouringIndex serviceState = labeledVBox
  "Machine"
  [ padLeft (Pad 2) $ padRight (Pad 2) $ padTop (Pad 0) $ padBottom (Pad 1) $ str $ mergeSprites
      [base, heat r, water r, beans r, spill r, cup s r, pouring !! pouringIndex]
  ]
 where
  s = serviceState
  r = readings $ driverState s
  heat DriverReadings {..} | waterTemperature > 75 = heat4
                           | waterTemperature > 60 = heat3
                           | waterTemperature > 45 = heat2
                           | waterTemperature > 30 = heat1
                           | otherwise             = base
  water DriverReadings {..} | waterLevel > 90 = water5
                            | waterLevel > 70 = water4
                            | waterLevel > 50 = water3
                            | waterLevel > 30 = water2
                            | waterLevel > 10 = water1
                            | otherwise       = base
  beans DriverReadings {..} | beanLevel > 40 = beans2
                            | beanLevel > 10 = beans1
                            | otherwise      = base
  spill DriverReadings { spillage } = if spillage > 4 then spillFull else spillEmpty
  cup ServiceState { cupFilled } DriverReadings { cupDetected } =
    if cupDetected then if cupFilled then cupFull else cupEmpty else noCup

maybeTaskWidget :: Maybe Task -> Widget a
maybeTaskWidget task = labeledVBox title $ header : (drawStep <$> maybe [] (steps . recipe) task)
 where
  suffix (Just t) = show (recipeType . recipe $ t)
  suffix Nothing  = "Nothing"
  title  = "Current Task (" ++ suffix task ++ ")"
  header = drawColumns "Step" ["Status"]
  drawStep :: RecipeStep -> Widget a
  drawStep RecipeStep { status, instruction } = drawColumns (show instruction) [show status]


debugWidget :: ServiceState -> Widget a
debugWidget ServiceState { driverState, cupFilled, desiredTemperature, currentTask } = labeledVBox
  "Service State"
  [ drawStatus "Cup filled"         cupFilled
  , drawStatus "DesiredTemperature" desiredTemperature
  , maybeTaskWidget currentTask
  , labeledVBox
    "Driver Readings"
    [ drawReading "Water temperature" waterTemperature
    , drawReading "Water level"       waterLevel
    , drawReading "Bean level"        beanLevel
    , drawReading "Cup detected"      cupDetected
    , drawReading "Spillage"          spillage
    , drawReading "Brewer position" $ brewerPosToState . brewerPosition
    ]
  , labeledVBox
    "Driver Commands"
    [ drawCommand "BoilerHeater"                  boilerHeater
    , drawCommand "Grinder"                       grinder
    , drawCommand "BrewerState"                   brewerState
    , drawCommand "WaterLowIndicator"             waterLowIndicator
    , drawCommand "Pump"                          pump
    , drawCommand "GroundsContainerFullIndicator" groundsContainerFullIndicator
    , drawCommand "BoilerOutValve"                boilerOutValve
    , drawCommand "BrewerOutValve"                brewerOutValve
    ]
  ]
 where
  drawState :: (Show v) => String -> (DriverState -> [v]) -> Widget a
  drawState key prop = drawColumns key (show <$> prop driverState)
  drawStatus :: (Show v) => String -> v -> Widget a
  drawStatus key value = drawState key $ const [value]
  drawReading :: (Show v) => String -> (DriverReadings -> v) -> Widget a
  drawReading key prop = drawState key (singleton . prop . readings)
  drawCommand :: (Show v) => String -> (DriverCommands -> v) -> Widget a
  drawCommand key prop = drawState key (singleton . prop . commands)

mergeSprites :: [String] -> String
mergeSprites = foldl1 $ zipWith (\x y -> if y == ' ' then x else y)

button :: String -> a -> Widget a
button label name = clickable name $ withBorderStyle BS.unicode $ B.border $ padLeftRight 1 $ str label

labeledVBox :: String -> [Widget a] -> Widget a
labeledVBox label content =
  withBorderStyle BS.unicodeBold $ B.borderWithLabel (padLeftRight 1 $ str label) $ vBox content

drawColumns :: String -> [String] -> Widget a
drawColumns x xs = foldl (<+>) (padLeftRight 1 $ str x) (padLeft Max . str <$> xs)

singleton :: a -> [a]
singleton x = [x]
