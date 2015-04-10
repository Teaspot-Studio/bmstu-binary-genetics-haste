{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Radio.Genetic where

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

import Radio.Task 
import Radio.Tower 

type Field = [[Int]]

newtype TowersIndivid = TowersIndivid [Bool]
  deriving (Show, Eq, Generic)

instance NFData TowersIndivid

instance Individ TowersIndivid where
  type IndividOptions TowersIndivid = Int

  crossover _ ia@(TowersIndivid a) ib@(TowersIndivid b)
    | length a /= length b = fail "Chromosomes have different lengths"
    | length a <= 1 = return (ia, ib)
    | length a == 2 = 
      let [a0, a1] = a 
          [b0, b1] = b
      in return (TowersIndivid [a0, b1], TowersIndivid [b0, a1])
    | otherwise = crossover3 ia ib

  mutation _ ind@(TowersIndivid []) = return ind
  mutation _ (TowersIndivid a) = do
    i <- getRandomR (0, length a - 1)
    let pre = take i a 
        post = drop (i + 1) a 
    return $ TowersIndivid $ pre ++ [not $ a !! i] ++ post
    
  initIndivid n = return . TowersIndivid =<< replicateM n getRandom

-- | Implements three point crossover. Length of chromosome must be > 3
crossover3 :: TowersIndivid -> TowersIndivid -> PauseableRand (TowersIndivid, TowersIndivid)
crossover3 (TowersIndivid a) (TowersIndivid b) = do
  [p1, p2, p3] <- sort <$> replicateM 3 (getRandomR (1, n - 2))
  let a' = concat [slice 0 p1 a, slice p1 p2 b, slice p2 p3 a, slice p3 n b]
  let b' = concat [slice 0 p1 b, slice p1 p2 a, slice p2 p3 b, slice p3 n a]
  return $ (TowersIndivid a', TowersIndivid b')
  where
    n = length a
    slice i1 i2 = take (i2 - i1) . drop i1

extractSolution :: Input -> GeneticState TowersIndivid -> Output
extractSolution input state = Output (filterTowers input ind) fit 
  where (fit, ind) = findBest (fitness input) $ geneticPopulations state

-- | Calculates fitness using user function
fitness :: Input -> TowersIndivid -> Float
fitness input ind = unsafePerformIO $
  userFunc coverage towerUsed towerGetter towerCount totalTowerGetter xsize ysize fieldGetter
  where
    (xsize, ysize) = inputFieldSize input
    towers = filterTowers input ind
    totalTowers = inputTowers input

    coverage = calcCoverage input ind
    towerUsed = length towers
    towerCount = length totalTowers

    towerGetter :: Int -> IO JSAny
    towerGetter i = return . toObject . toJSON $ towers !! i 

    totalTowerGetter :: Int -> IO JSAny
    totalTowerGetter i = return . toObject . toJSON $ totalTowers !! i 

    fieldGetter :: Int -> Int -> IO Int 
    fieldGetter x y = return $ (solutionField input towers !! x) !! y

    userFunc :: Float -- ^ Coverage
      -> Int -- ^ count of used towers
      -> (Int -> IO JSAny) -- ^ returns used tower by index
      -> Int -- ^ total count of towers
      -> (Int -> IO JSAny) -- ^ returns tower by index from all towers
      -> Int -- ^ field width
      -> Int -- ^ field height
      -> (Int -> Int -> IO Int) -- ^ returns count of towers that covers the cell
      -> IO Float
    userFunc = ffi $ toJSStr $ "(" ++ inputFitness input ++ ")"

-- | Calculates coverage of field by a solution
calcCoverage :: Input -> TowersIndivid -> Float 
calcCoverage input = calcCoverage' input . filterTowers input

calcCoverage' :: Input -> [Tower] -> Float 
calcCoverage' input = coverage . solutionField input
  where
    toFloat :: Int -> Float
    toFloat = fromIntegral . toInteger

    coverage :: Field -> Float
    coverage f = toFloat covered / toFloat area
      where
        covered = foldl' (\i b -> if b > 0 then i+1 else i) 0 $ concat f
        area = length $ concat f

-- | Builds field, each cell contains count of towers that covers the cell
solutionField :: Input -> [Tower] -> Field 
solutionField input towers = makeRow <$> [0 .. height]
  where
    (width, height) = inputFieldSize input
    makeRow y = (\x -> sum $ bool2int . inRadius x y <$> towers) <$> [0 .. width]

    bool2int b = if b then 1 else 0

    inRadius :: Int -> Int -> Tower -> Bool
    inRadius x y t = dx*dx + dy*dy <= r*r
      where 
        r = fromIntegral (towerRadius t) + 0.5
        dx = fromIntegral $ towerX t - x 
        dy = fromIntegral $ towerY t - y

-- | Returns only placed towers by solution in chromosome
filterTowers :: Input -> TowersIndivid -> [Tower]
filterTowers input (TowersIndivid ind) = fmap snd $ filter ((== True) . fst) $ zip ind $ inputTowers input