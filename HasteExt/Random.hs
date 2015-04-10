module HasteExt.Random where

import Control.Arrow
import Control.Applicative
import System.Random as SR
import Haste as Haste

newtype HasteGen = HasteGen Seed

instance Show HasteGen where
  show _ = "HasteGen"

instance RandomGen HasteGen where
  next (HasteGen seed) = second HasteGen $ Haste.randomR (0, maxBound-1) seed
  split g@(HasteGen seed) = (g, HasteGen $ Haste.next seed)
  genRange _ = (0, maxBound-1)

newHasteGen :: IO HasteGen
newHasteGen = return . HasteGen =<< newSeed