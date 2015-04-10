module Radio.Plot where

import Haste.Graphics.Canvas
import Haste.Perch hiding (head)
import Haste.HPlay.View hiding (head)
import Prelude hiding (id, div)
import Text.Printf
import Data.Function hiding (id)
import Data.List
import Control.Monad.IO.Class (liftIO)
import Control.Arrow
import Control.Applicative

import Radio.Task
import Debug.Trace 

plotWidget :: PlotState -> String -> String -> (Double, Double) -> Widget ()
plotWidget state xstr ystr (xsize, ysize) = do 
  canvasId <- fmap ("canvas" ++) getNextId
  resetEventData
  wraw $ do 
    canvas ! id canvasId
           ! style "border: 1px solid black;"
           ! atr "width" (show $ (1 + 2 * margin) * xsize)
           ! atr "height" (show $ (1 + 2 * margin) * ysize)
           $ noHtml
  wraw $ liftIO $ do 
    wcan <- getCanvasById canvasId 
    case wcan of 
      Nothing -> return ()
      Just can -> render can $
        translate (margin*xsize, margin*ysize) $ 
        plot xstr ystr points (xsize, ysize)
  where
    points = second toDouble . first fromIntegral <$> values state
    margin = 0.1

    toDouble :: Float -> Double 
    toDouble = fromRational . toRational

plot :: String -> String -> [(Double, Double)] -> (Double, Double) -> Picture ()
plot xstr ystr pts (xs, ys) = coords >> xlabel >> ylabel >> grid >> plotted
  where
    coords = xcoord >> ycoord
    xcoord = stroke (line (0, ys) (xs, ys)) >> translate (xs, ys) xarrow
    ycoord = stroke (line (0, ys) (0, 0)) >> translate (0, 0) yarrow
    xarrow = stroke $ path [(-0.025*xs, 0.01*ys), (0, 0), (-0.025*xs, -0.01*ys)]
    yarrow = rotate (-pi/2) xarrow

    xmin = fst $ minimumBy (compare `on` fst) pts
    xmax = fst $ maximumBy (compare `on` fst) pts
    ymin = snd $ minimumBy (compare `on` snd) pts
    ymax = snd $ maximumBy (compare `on` snd) pts

    xmargin = 0.1
    ymargin = 0.1
    xrange = let v = xmax - xmin in if v < 0.001 then 1.0 else v
    yrange = let v = ymax - ymin in if v < 0.001 then 1.0 else v
    toLocal (x, y) = ( xs * (xmargin + (1 - 2*xmargin) * (x - xmin) / xrange)
                     , ys * (1 - (ymargin + (1 - 2*ymargin) * (y - ymin) / yrange)))
    fromLocalX x = xmin + (x / xs - xmargin) * xrange / (1 - 2*xmargin)
    fromLocalY y = ymin + ((1 - y / ys) - ymargin) * yrange / (1 - 2*ymargin)   

    localPts = toLocal <$> pts
    intervals 
      | null localPts = [] 
      | length localPts == 1 = [(head localPts, head localPts)] 
      | otherwise = localPts `zip` tail localPts
    plotted = color red $ sequence_ $ stroke . uncurry line <$> intervals
    red = RGB 200 0 0

    ltexscale = 2.0 * xs / 900
    xlabel = translate (0.8*xs, 0.95*ys) $ scale (ltexscale, ltexscale) $ text (0,0) xstr
    ylabel = translate (0.05*xs, 0) $ scale (ltexscale, ltexscale) $ text (0,0) ystr

    gridPts 
      | length localPts <= 2 = localPts
      | otherwise = head localPts : (middle ++ [last localPts])
      where
        middlePts = tail $ init localPts
        middle = takeUniform (min 10 (length middlePts)) middlePts
        
    smalltext :: Int -> Double -> Picture ()
    smalltext n = scale (ltexscale, ltexscale) . text (0, 0) . truncText n
    truncText :: Int -> Double -> String
    truncText n d =  if v == "-0." ++ replicate n '0' then "0." ++ replicate n '0' else v
      where v = printf ("%."++show n++"f") d

    xgrid = sequence_ $ (\x -> stroke (line (x,0) (x,ys)) >> translate (x - (textOffsetX x)*0.005*xs, 1.05*ys) (smalltext 0 $ fromLocalX x)) . fst <$> gridPts
    ygrid = sequence_ $ (\y -> stroke (line (0,y) (xs,y)) >> translate (  - (textOffsetY y)*0.022*xs, y+0.015*ys) (smalltext 2 $ fromLocalY y)) . snd <$> gridPts
    textOffsetX n = min 4 $ (fromIntegral $ length (truncText 0 $ fromLocalX n) - 1)
    textOffsetY n = min 4 $ (fromIntegral $ length (truncText 2 $ fromLocalY n) - 1)

    grid = color (RGB 125 125 125) $ xgrid >> ygrid

takeUniform :: Int -> [a] -> [a]
takeUniform n l
  | n > length l = error "n is larger than passed list!"
  | otherwise = take n $ every step l 
  where
    step = round $ (fromIntegral (length l) :: Float) / fromIntegral n
    every k xs = case drop (k-1) xs of
              (y:ys) -> y : every k ys
              [] -> []

sample :: Float -> Float -> Int -> (Float -> Float) -> [(Float, Float)]
sample xmin xmax i f = (\x -> (x, f x)) <$> ((\j -> xmin + (xmax - xmin) * fromIntegral j / fromIntegral i) <$> [0 .. i - 1])               