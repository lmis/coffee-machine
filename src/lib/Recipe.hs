module Recipe
  ( Recipe(..)
  , RecipeType(..)
  , RecipeStep(..)
  , RecipeStatus(..)
  , RecipeInstruction(..)
  )
where

data RecipeType = Beverage | Cleanup deriving (Eq, Show)
data RecipeInstruction
  =
  DesiredTemperature { target :: Double}
  | Grind { grams :: Double }
  | DropGrounds
  | Extract { milliliters :: Double }
  deriving (Eq, Show, Ord)

data RecipeStatus = Pending | Doing | Done | Aborted
  deriving (Eq, Show, Ord)

data RecipeStep = RecipeStep {
  index :: Integer,
  instruction :: RecipeInstruction,
  status :: RecipeStatus
} deriving (Eq, Show)


data Recipe = Recipe {
  recipeType::RecipeType,
  steps::[RecipeStep]
}
