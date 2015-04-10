{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Binary.Task where

import Haste.Serialize
import Haste.JSON
import Data.Typeable
import Data.Maybe
import Control.Applicative
import Genetic.Options

data Input = Input {
  inputDigitsPrevDot :: Int,
  inputDigitsCount :: Int,
  inputExpected :: Double,
  inputFitness :: String, -- ^ JS expression
  inputGeneticOptions :: GeneticOptions
} deriving (Typeable, Show)

instance Serialize Input where
  toJSON i = Dict [
      ("inputDigitsPrevDot", toJSON $ inputDigitsPrevDot i)
    , ("inputDigitsCount", toJSON $ inputDigitsCount i)
    , ("inputExpected", toJSON $ inputExpected i)
    , ("inputFitness", toJSON $ inputFitness i)
    , ("inputGeneticOptions", toJSON $ inputGeneticOptions i)
    ] 
  parseJSON j = Input 
    <$> j .: "inputDigitsPrevDot"
    <*> j .: "inputDigitsCount"
    <*> j .: "inputExpected"
    <*> j .: "inputFitness"
    <*> j .: "inputGeneticOptions"

initialInput :: Input
initialInput = Input {
    inputDigitsPrevDot = 3,
    inputDigitsCount = 10,
    inputExpected = 3,
    inputFitness = "function(x)\n{\n    return x*Math.sin(x);\n}",
    inputGeneticOptions = initialOptions
  }

data Output = Output {
  outputSolution :: Double,
  outputFitness :: Double
} deriving (Typeable, Show)

data PlotState = PlotState{
  values :: [(Double, Double)] -- ^ Points: x - generation number, y - fitness value
} deriving (Typeable, Show)

initialPlotState :: PlotState 
initialPlotState = PlotState []