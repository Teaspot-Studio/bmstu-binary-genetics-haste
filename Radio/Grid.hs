module Radio.Grid where

import Haste.Graphics.Canvas

data Grid = Grid {
  drawGrid :: Picture (),
  pixelToCell :: (Int, Int) -> (Int, Int),
  cellToPixel :: (Int, Int) -> Point
}

grid :: Int -> Int -> Double -> (Int -> Int -> Color) -> Grid
grid xsize ysize cellSize cellColor = Grid {
    drawGrid = makeColumns
  , pixelToCell = \(x, y) -> (
      floor (fromIntegral x / cellSize), 
      floor (fromIntegral y / cellSize))
  , cellToPixel = \(cx, cy) -> (
      fromIntegral cx * cellSize, 
      fromIntegral cy * cellSize)
  }
  where
    makeColumns = mapM_ (\row -> makeLine row) [0 .. ysize-1]
    makeLine row = mapM_ (\col -> cell col row) [0 .. xsize-1]
      where 
        cell x y = 
          let dx = fromIntegral x * cellSize
              dy = fromIntegral y * cellSize
              c = cellColor x y
              shape = rect (0, 0) (cellSize, cellSize)
              body = color c $ fill shape 
              contour = stroke shape
          in translate (dx, dy) $ body >> contour