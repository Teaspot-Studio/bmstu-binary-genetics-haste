{-# LANGUAGE OverloadedStrings #-}
module Radio.Tower where

import Haste.Graphics.Canvas
import Haste.Serialize
import Haste.JSON
import Control.Applicative

data Tower = Tower {
  towerX :: Int,
  towerY :: Int,
  towerRadius :: Int
} deriving (Show)

instance Eq Tower where
  t1 == t2 = towerX t1 == towerX t2 && towerY t1 == towerY t2
  
instance Serialize Tower where
  toJSON t = Dict [
      ("towerX", toJSON $ towerX t)
    , ("towerY", toJSON $ towerY t)
    , ("towerRadius", toJSON $ towerRadius t)
    ]
  parseJSON json = Tower 
    <$> json .: "towerX"
    <*> json .: "towerY"
    <*> json .: "towerRadius"

tower :: Vector -> Color -> Picture ()
tower size baseColor = scale size $ triangle >> dot >> rwaves >> lwaves
  where
    triangle = color baseColor $ fill $ path [(0.2, 1), (0.8, 1), (0.5, 0.5)]
    dot = color baseColor $ fill $ circle (0.5, 0.3) 0.1
    waves a = translate (0.5, 0.3) $ rotate a $ do
      thickArc baseColor 0 (2*pi/3) 0.25 0.05
      thickArc baseColor 0 (2*pi/3) 0.4 0.05
    rwaves = waves (-pi/3)
    lwaves = waves (2*pi/3)

thickArc :: Color -> Double -> Double -> Double -> Double -> Picture ()
thickArc baseColor startAngle endAngle startRad thickness = color baseColor $ 
  fill $ path $ innerPoints ++ reverse outerPoints ++ [head innerPoints]
  where
    pointsCount = 20 :: Int
    endRad = startRad + thickness
    stepAngle i = startAngle + (fromIntegral i)*(endAngle-startAngle) / fromIntegral pointsCount
    angles = fmap stepAngle [0 .. pointsCount-1]
    makePoints r = fmap (\a -> (r * cos a, r * sin a)) angles
    innerPoints = makePoints startRad
    outerPoints = makePoints endRad