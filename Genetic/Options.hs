{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Genetic.Options where

import Haste.Serialize
import Haste.JSON
import Data.Typeable
import Data.Maybe
import Control.Applicative

data GeneticOptions = GeneticOptions {
  mutationChance :: Float,
  elitePart :: Float,
  maxGeneration :: Int,
  popCount :: Int,
  indCount :: Int,
  targetFitness :: Maybe Float
} deriving (Typeable, Show)

instance Serialize GeneticOptions where
  toJSON o = Dict $ [
      ("mutationChance", toJSON $ mutationChance o)
    , ("elitePart", toJSON $ elitePart o)
    , ("maxGeneration", toJSON $ maxGeneration o)
    , ("popCount", toJSON $ popCount o)
    , ("indCount", toJSON $ indCount o)
    ] ++ if isJust $ targetFitness o 
      then [("targetFitness", toJSON $ fromJust $ targetFitness o)]
      else []

  parseJSON j = GeneticOptions
    <$> j .: "mutationChance"
    <*> j .: "elitePart"
    <*> j .: "maxGeneration"
    <*> j .: "popCount"
    <*> j .: "indCount"
    <*> j .:? "targetFitness"

initialOptions :: GeneticOptions
initialOptions = GeneticOptions {
    mutationChance = 0.3,
    elitePart = 0.1,
    maxGeneration = 25,
    popCount = 2,
    indCount = 10,
    targetFitness = Nothing
  }