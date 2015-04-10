{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Radio.Task where

import Radio.Tower 
import Haste.Serialize
import Haste.JSON
import Data.Typeable
import Data.Maybe
import Control.Applicative
import Genetic.Options

data Input = Input {
  inputFieldSize :: (Int, Int),
  inputTowers :: [Tower],
  inputRadius :: Int,
  inputFitness :: String, -- ^ JS expression
  inputGeneticOptions :: GeneticOptions
} deriving (Typeable, Show)

instance Serialize Input where
  toJSON i = Dict [
      ("inputFieldSize", toJSON $ inputFieldSize i)
    , ("inputTowers", toJSON $ inputTowers i)
    , ("inputRadius", toJSON $ inputRadius i)
    , ("inputFitness", toJSON $ inputFitness i)
    , ("inputGeneticOptions", toJSON $ inputGeneticOptions i)
    ] 
  parseJSON j = Input 
    <$> j .: "inputFieldSize"
    <*> j .: "inputTowers"
    <*> j .: "inputRadius"
    <*> j .: "inputFitness"
    <*> j .: "inputGeneticOptions"

initialInput :: Input
initialInput = Input {
    inputFieldSize = (20, 20),
    inputTowers = [],
    inputRadius = 3,
    inputFitness = "function(coverage, usedCount, towerUsedGetter, totalCount, towerTotalGetter, fieldWidth, fieldHeight, fieldGetter)\n{\n    return coverage*(1 - usedCount / totalCount);\n}",
    inputGeneticOptions = initialOptions
  }

data Output = Output {
  outputTowers :: [Tower],
  outputFitness :: Float
} deriving (Typeable, Show)

data PlotState = PlotState{
  values :: [(Int, Float)] -- ^ Points: x - generation number, y - fitness value
} deriving (Typeable, Show)

initialPlotState :: PlotState 
initialPlotState = PlotState []