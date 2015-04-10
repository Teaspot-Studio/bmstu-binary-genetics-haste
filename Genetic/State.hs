module Genetic.State where

import Control.Applicative
import Genetic.Population
import HasteExt.Random

data GeneticState a = GeneticState {
  geneticFinished :: Bool,
  geneticCurrentGen :: Int,
  geneticPopulations :: [Population a],
  geneticCurrentBest :: Maybe (Double, a),
  geneticGen :: HasteGen
}

initialGeneticState :: IO (GeneticState a)
initialGeneticState = GeneticState False 0 [] Nothing <$> newHasteGen

isGeneticFinished :: GeneticState a -> Bool 
isGeneticFinished = geneticFinished