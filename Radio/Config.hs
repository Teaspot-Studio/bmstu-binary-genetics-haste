{-# LANGUAGE OverloadedStrings #-}
module Radio.Config where

import Prelude hiding (id, div)
import Haste hiding (style)
import Haste.Perch hiding (head)
import Haste.HPlay.View hiding (head)
import Control.Monad.IO.Class (liftIO)
import Data.Monoid

import Genetic.Options
import Radio.Field
import Radio.Task 
import Radio.Util

fieldConfigWidget :: Input -> Widget Input
fieldConfigWidget input = do
  --writeLog $ show $ inputTowers input
  (dwidth, _) <- liftIO $ getDocumentSize
  let (xsize, ysize) = inputFieldSize input
      cellSize = fromIntegral dwidth * 0.4 / fromIntegral xsize
  div ! atr "class" "row vertical-align" <<<
    (   ((div ! atr "class" "col-md-1" $ noHtml) ++>
        (div ! atr "class" "col-md-5" <<< editingCntl))
    <|> div ! atr "class" "col-md-6" <<< field cellSize)
  where
    
    field = fieldConfig input 

    bsrow = div ! atr "class" "row"

    editingCntl :: Widget Input
    editingCntl = bsrow <<<
          (fieldOptionsCnt <|> evolOptionsCnt <|> fitnessCntl) 
      where
        fieldOptionsCnt = (bsrow $ label ("Настройки поля: " :: JSString) ! atr "style" "font-size: 20px") ++>
          (bsrow <<< (radiusCntl <|> fieldWidthCntl <|> fieldHeightCntl))

    makeCounter :: Int -> JSString -> JSString -> Widget Int
    makeCounter initial labelStr errmsg = bsrow <<< 
      ((div ! atr "class" "col-md-6" $ label (labelStr :: JSString)) ++>
       (div ! atr "class" "col-md-6" <<< (incBtn <|> f <|> decBtn)) ) 
      `validate` (\r -> return $ if r > 0 then Nothing else Just $ b (errmsg :: JSString))  
      where
        f = inputInt (Just initial) ! atr "size" "2" `fire` OnKeyUp
        incBtn = cbutton (initial + 1) "+" `fire` OnClick
        decBtn = cbutton (initial - 1) "-" `fire` OnClick

    radiusCntl :: Widget Input
    radiusCntl = do
      newRadius <- makeCounter (inputRadius input) "Радиус: " "радиус должен быть положителен"
      return $ input {
        inputRadius = newRadius
      }

    fieldWidthCntl :: Widget Input 
    fieldWidthCntl = do
      newWidth <- makeCounter (fst $ inputFieldSize input) "Ширина поля: " "ширина должна быть положительна"
      return $ input {
        inputFieldSize = (newWidth, snd $ inputFieldSize input)
      } 

    fieldHeightCntl :: Widget Input 
    fieldHeightCntl = do
      newHeight <- makeCounter (snd $ inputFieldSize input) "Высота поля: " "высота должна быть положительна"
      return $ input {
        inputFieldSize = (fst $ inputFieldSize input, newHeight)
      } 

    fitnessCntl :: Widget Input
    fitnessCntl = do
      newFitness <- bsrow <<< (
        (bsrow $ label ("Фитнес функция: " :: JSString) ! atr "style" "margin-top: 40px; font-size: 20px") ++>
        (bsrow <<< textArea (inputFitness input) ! atr "rows" "6" ! atr "cols" "60" <++ br 
          <** (inputSubmit "Обновить" `fire` OnClick <! [atr "style" "margin-bottom: 40px"])) <++
        (bsrow $ panel "Пояснения к параметрам:" $ mconcat [
            labelRow 2 "coverage:" "число с плавающей запятой, процент клеток поля, покрытых хотя бы одной вышкой"
          , labelRow 2 "usedCount:" "целочисленное, количество использованных башен"
          , labelRow 2 "towerUsedGetter:" "функция, берующая индекс башни и возвращающая использованную башню в виде объекта {int towerX, int towerY, int towerRadius}, максимальный индекс usedCount-1, минимальный индекс 0"
          , labelRow 2 "totalCount:" "целочисленное, количество башен на поле (включая неиспользованные в данном решении)"
          , labelRow 2 "towerTotalGetter:" "функция, берующая индекс башни (любая на поле) и возвращающая башню в виде объекта {int towerX, int towerY, int towerRadius}, максимальный индекс totalCount-1, минимальный индекс 0"
          , labelRow 2 "fieldWidth:" "целочисленное, ширина поля"
          , labelRow 2 "fieldHieght:" "целочисленное, высота поля"
          , labelRow 2 "fieldGetter:" "функция, берущая два индекса (x и y), соответствующие ячейке поля (x может принимать значения от 0 до fieldWidth-1, y может принимать значения от 0 до fieldHieght-1). Возвращает количество башен, покрывающих данную ячейку (x,y)"
          ]))
      return $ input {
        inputFitness = newFitness
      }

    evolOptionsCnt :: Widget Input 
    evolOptionsCnt = do
      newOptions <- bsrow <<< (label ("Настройки эволюции:" :: JSString) ! atr "style" "margin-top: 40px; font-size: 20px" 
        ++> evolOptionsCnt')
      --liftIO $ writeLog $ show newOptions
      return $ input {
        inputGeneticOptions = newOptions
      }
      where
        options = inputGeneticOptions input

        evolOptionsCnt' :: Widget GeneticOptions 
        evolOptionsCnt' = GeneticOptions <$> mutChanceCnt <*> elitePartCnt <*> maxGenCnt <*> popCountCnt <*> indCountCnt <*> pure Nothing
          <** inputSubmit "Обновить" `fire` OnClick

        mutChanceCnt :: Widget Float
        mutChanceCnt = bsrow <<< (
          (div ! atr "class" "col-md-6" $ label ("Шанс мутации: " :: JSString)) ++>
          (div ! atr "class" "col-md-6" <<< inputFloat (Just $ mutationChance options)
          `validate`
          (\c -> return $ if c >= 0.0 && c <= 1.0 then Nothing else Just $ b ("вероятность некорректна [0, 1]" :: JSString))))

        elitePartCnt :: Widget Float
        elitePartCnt = bsrow <<< (
          (div ! atr "class" "col-md-6" $ label ("Часть элиты: " :: JSString)) ++>
          (div ! atr "class" "col-md-6" <<< inputFloat (Just $ elitePart options)
          `validate`
          (\c -> return $ if c >= 0.0 && c <= 1.0 then Nothing else Just $ b ("доля некорректна [0, 1]" :: JSString))))

        maxGenCnt :: Widget Int
        maxGenCnt = bsrow <<< (
          (div ! atr "class" "col-md-6" $ label ("Макс поколений: " :: JSString)) ++>
          (div ! atr "class" "col-md-6" <<< inputInt (Just $ maxGeneration options)
          `validate`
          (\c -> return $ if c > 0 then Nothing else Just $ b ("должно быть положительно" :: JSString))))

        popCountCnt :: Widget Int
        popCountCnt = bsrow <<< (
         (div ! atr "class" "col-md-6" $ label ("Число популяций: " :: JSString)) ++>
         (div ! atr "class" "col-md-6" <<< inputInt (Just $ popCount options)
         `validate`
         (\c -> return $ if c > 0 then Nothing else Just $ b ("должно быть положительно" :: JSString))))

        indCountCnt :: Widget Int
        indCountCnt = bsrow <<< (
         (div ! atr "class" "col-md-6" $ label ("Число индивидов в популяции: " :: JSString)) ++>
         (div ! atr "class" "col-md-6" <<< inputInt (Just $ indCount options)
         `validate`
         (\c -> return $ if c > 0 then Nothing else Just $ b ("должно быть положительно" :: JSString))))
