module Radio.Field where

import Haste hiding (style)
import Haste.Graphics.Canvas
import Haste.HPlay.View hiding (head)
import Prelude hiding (id, div)
import Control.Monad.IO.Class
import Data.Foldable (find)

import Radio.Grid
import Radio.Tower 
import Radio.Task 
import Radio.Util
import Radio.Genetic 

fieldConfig :: Input -> Double -> Widget Input
fieldConfig input cellSize = do
  let g = grid xsize ysize cellSize $ const.const $ RGB 255 255 255 -- render grid
      gridOffset = (cellSize, cellSize)
      scaledText s pos t = translate pos $ scale (s, s) $ text (0, 0) t
      tw t = cellSize * (fromIntegral $ length (show t) - 1)
      xlabels = mapM_ (\x -> scaledText (4*cellSize/50) (fromIntegral x * cellSize + 0.25*cellSize - 0.3*(tw x), 0.8*cellSize) $ show x) [1 .. xsize]
      ylabels = mapM_ (\y -> scaledText (4*cellSize/50) (0.3*cellSize-0.3*(tw y), fromIntegral y * cellSize + 0.8*cellSize) $ show y) [1 .. ysize]
      viewWidth = fst gridOffset + cellSize * fromIntegral xsize + 5
      viewHeight = snd gridOffset + cellSize * fromIntegral ysize + 5
      margin = 0.1
      scaleToCell = scale (1, 1 - 2*margin)
      placeToCell = translate (0.35*cellSize/2, margin*cellSize)
      drawTower t = do
        placeToCell $ scaleToCell $ tower (0.65*cellSize, cellSize) (RGB 0 0 0)
        translate (cellSize/2, cellSize/2) $ stroke $ circle (0, 0) (cellSize * (0.5 + fromIntegral (towerRadius t)))
      placeTower t = translate (fst gridOffset + fromIntegral (towerX t) * cellSize
                              , snd gridOffset + fromIntegral (towerY t) * cellSize)
      drawTowers = mapM_ (\t -> placeTower t $ drawTower t) towers

  canvasId <- fmap ("canvas" ++) getNextId
  resetEventData
  wraw (do
    canvas ! id canvasId
        -- ! style "border: 1px solid black;" 
           ! atr "width" (show viewWidth)
           ! height (show viewHeight)
           $ noHtml)
    `fire` OnClick
  
  wraw $ liftIO $ do 
    wcan <- getCanvasById canvasId
    case wcan of 
      Nothing -> return ()
      Just can -> render can $ do
        translate gridOffset $ drawGrid g
        xlabels
        ylabels
        drawTowers

  e@(EventData typ _) <- getEventData
  evdata <- continueIf (evtName OnClick == typ) e

  offset <- liftIO $ getElementPosition $ "#" ++ canvasId
  -- alert $ show offset 

  mousePos <- liftIO getMousePosition
  let cell = toCell offset g mousePos
  -- alert $ show $ evData evdata
  -- writeLog $ "Cell " ++ show cell ++ " inbounds: " ++ show (inBounds cell)

  let newTowers = if inBounds cell then updateTowers cell else towers
  return $ input { inputTowers = newTowers }
  where
    (xsize, ysize) = inputFieldSize input
    towers = filter (\t -> inBounds (towerX t, towerY t)) $ inputTowers input

    toCell :: (Int, Int) -> Grid -> (Int, Int) -> (Int, Int)
    toCell (ofx, ofy) g (mx, my) = (cx -1, cy -1)
      where (cx, cy) = pixelToCell g (mx-ofx, my-ofy)

    inBounds :: (Int, Int) -> Bool
    inBounds (cx, cy) = cx >= 0 && cy >= 0 && cx < xsize && cy < ysize

    updateTowers :: (Int, Int) -> [Tower]
    updateTowers (x, y) = 
      case mt of
        Just t -> filter (\t -> towerX t /= x || towerY t /= y) towers
        Nothing -> Tower x y (inputRadius input) : towers 
      where
        mt = find (\t -> towerX t == x && towerY t == y) towers


fieldShow :: Input -> Output -> Double -> Widget ()
fieldShow input output cellSize = do
  let g = grid xsize ysize cellSize cellColor -- render grid
      gridOffset = (cellSize, cellSize)
      scaledText s pos t = translate pos $ scale (s, s) $ text (0, 0) t
      tw t = cellSize * (fromIntegral $ length (show t) - 1)
      xlabels = mapM_ (\x -> scaledText (4*cellSize/50) (fromIntegral x * cellSize + 0.25*cellSize - 0.3*(tw x), 0.8*cellSize) $ show x) [1 .. xsize]
      ylabels = mapM_ (\y -> scaledText (4*cellSize/50) (0.3*cellSize-0.3*(tw y), fromIntegral y * cellSize + 0.8*cellSize) $ show y) [1 .. ysize]
      viewWidth = fst gridOffset + cellSize * fromIntegral xsize + 5
      viewHeight = snd gridOffset + cellSize * fromIntegral ysize + 5
      margin = 0.1
      scaleToCell = scale (1, 1 - 2*margin)
      placeToCell = translate (0.35*cellSize/2, margin*cellSize)
      drawTower t c = do
        placeToCell $ scaleToCell $ tower (0.65*cellSize, cellSize) c
        translate (cellSize/2, cellSize/2) $ color c $ stroke $ circle (0, 0) (cellSize * (0.5 + fromIntegral (towerRadius t)))
      placeTower t = translate (fst gridOffset + fromIntegral (towerX t) * cellSize
                              , snd gridOffset + fromIntegral (towerY t) * cellSize)
      drawTowers c = mapM_ (\t -> placeTower t $ drawTower t c)

  canvasId <- fmap ("canvas" ++) getNextId
  resetEventData
  wraw (do
    canvas ! id canvasId
        -- ! style "border: 1px solid black;" 
           ! atr "width" (show viewWidth)
           ! height (show viewHeight)
           $ noHtml)
    `fire` OnClick
  
  wraw $ liftIO $ do 
    wcan <- getCanvasById canvasId
    case wcan of 
      Nothing -> return ()
      Just can -> render can $ do
        translate gridOffset $ drawGrid g
        xlabels
        ylabels
        drawTowers (RGB 230 230 230) unplacedTowers
        drawTowers (RGB 0 0 0) placedTowers

  where
    (xsize, ysize) = inputFieldSize input
    unplacedTowers = filter (\t -> inBounds (towerX t, towerY t) && not (t `elem` placedTowers)) $ inputTowers input
    placedTowers = filter (\t -> inBounds (towerX t, towerY t)) $ outputTowers output

    inBounds :: (Int, Int) -> Bool
    inBounds (cx, cy) = cx >= 0 && cy >= 0 && cx < xsize && cy < ysize

    cellColor :: Int -> Int -> Color 
    cellColor x y = if value == 0 
      then RGB 255 255 255
      else RGB 0 (clamp 100 250 $ 250 - value * 50) 0
      where
        clamp minv maxv = max minv . min maxv
        value = (solutionField input placedTowers !! y ) !! x