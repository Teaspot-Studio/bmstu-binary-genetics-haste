{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Binary.Genetic where

import Data.List
import Control.Applicative
import Control.Monad 
import Control.Monad.Random 
import GHC.Generics (Generic)
import Control.DeepSeq
import System.IO.Unsafe 
import Haste.Foreign
import Haste.Prim
import Haste.JSON
import Haste.Serialize
import Haste

import Genetic.Coroutine 
import Genetic.Individ
import Genetic.State 
import Genetic.Population 

import Binary.Task
import Binary.Util

newtype FuncIndivid = FuncIndivid [Int]
  deriving (Show, Eq, Generic)

instance NFData FuncIndivid

instance Individ FuncIndivid where
  type IndividOptions FuncIndivid = Int

  crossover _ ia@(FuncIndivid a) ib@(FuncIndivid b)
    | length a /= length b = fail "Chromosomes have different lengths"
    | length a <= 1 = return (ia, ib)
    | length a == 2 = 
      let [a0, a1] = a 
          [b0, b1] = b
      in return (FuncIndivid [a0, b1], FuncIndivid [b0, a1])
    | otherwise = crossover3 ia ib

  mutation _ ind@(FuncIndivid []) = return ind
  mutation _ (FuncIndivid chr) = do 
    n <- uniform [0..length chr - 1]
    return $ FuncIndivid $ invertAt n chr
    where invertAt n orig = take n orig ++ [el] ++ drop (n+1) orig
            where el = if orig !! n == 0 then 1 :: Int else 0

  initIndivid n = return . FuncIndivid =<< replicateM n ((\b -> if b then 1 else 0) <$> getRandom)

-- | Implements three point crossover. Length of chromosome must be > 3
crossover3 :: FuncIndivid -> FuncIndivid -> PauseableRand (FuncIndivid, FuncIndivid)
crossover3 (FuncIndivid a) (FuncIndivid b) = do
  [p1, p2, p3] <- sort <$> replicateM 3 (getRandomR (1, n - 2))
  let a' = concat [slice 0 p1 a, slice p1 p2 b, slice p2 p3 a, slice p3 n b]
  let b' = concat [slice 0 p1 b, slice p1 p2 a, slice p2 p3 b, slice p3 n a]
  return $ (FuncIndivid a', FuncIndivid b')
  where
    n = length a
    slice i1 i2 = take (i2 - i1) . drop i1

extractSolution :: Input -> GeneticState FuncIndivid -> Maybe Output
extractSolution input state = case findBest (fitness input) $ geneticPopulations state of
    Just (fit, FuncIndivid ind) -> Just $ Output (binToDouble (fromIntegral $ inputDigitsPrevDot input) ind) fit 
    Nothing -> Nothing

-- | Calculates fitness using user function
fitness :: Input -> FuncIndivid -> Double
fitness input = fitGenerator (fromIntegral $ inputDigitsPrevDot input) (userFunction input) (inputExpected input)
  where
    fitGenerator :: Double -> (Double -> Double) -> Double -> FuncIndivid -> Double
    fitGenerator num f target (FuncIndivid chromosome) = 
      let x = binToDouble num chromosome
          delta = target - f x :: Double
      in (1.0 / delta ** 2.0)

userFunction :: Input -> Double -> Double 
userFunction input x = unsafePerformIO $ ffi (toJSStr $ "(" ++ inputFitness input ++ ")") x