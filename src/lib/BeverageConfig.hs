module BeverageConfig
  ( BeverageConfig(..)
  , beverages
  )
where

import           Recipe                         ( RecipeInstruction(..) )

data BeverageConfig = BeverageConfig {
  name :: String,
  instructions :: [RecipeInstruction]
} deriving (Show, Eq, Ord)

beverages :: [BeverageConfig]
beverages =
  [ BeverageConfig { instructions = [DesiredTemperature 85, Grind 15, Extract 30], name = "Espresso" }
  , BeverageConfig { instructions = [DesiredTemperature 85, Grind 15, Extract 30, DropGrounds, Grind 15, Extract 30]
                   , name         = "Dopio"
                   }
  ]

