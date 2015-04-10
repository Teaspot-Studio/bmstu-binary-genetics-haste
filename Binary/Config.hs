{-# LANGUAGE OverloadedStrings #-}
module Binary.Config where

import Prelude hiding (id, div)
import Haste hiding (style)
import Haste.Perch hiding (head)
import Haste.HPlay.View hiding (head)
import Control.Monad.IO.Class (liftIO)
import Data.Monoid
import Data.Maybe

import Genetic.Options
import Binary.Task 
import Binary.Util

fieldConfigWidget :: Input -> Widget Input
fieldConfigWidget input = do
  --writeLog $ show $ inputTowers input
  --(dwidth, _) <- liftIO $ getDocumentSize
  div ! atr "class" "row vertical-align" <<<
    (   ((div ! atr "class" "col-md-1" $ noHtml) ++>
        (div ! atr "class" "col-md-11" <<< editingCntl)))
  where
    bsrow = div ! atr "class" "row"
    bsrowFluid = div ! atr "class" "row-fluid"

    editingCntl :: Widget Input
    editingCntl = bsrowFluid <<<
          (fieldOptionsCnt <|> evolOptionsCnt <|> fitnessCntl) 
      where
        fieldOptionsCnt = (bsrow $ label ("Настройки функции: " :: JSString) ! atr "style" "font-size: 20px") ++>
          (bsrow <<< (digitsCountCnt <|> digitsPreDotCountCnt <|> expectedCnt))

    makeCounter' :: Int -> JSString -> (Int -> Maybe String) -> Widget Int
    makeCounter' initial labelStr validation = bsrow <<< 
      ((div ! atr "class" "col-md-6" $ label (labelStr :: JSString)) ++>
       (div ! atr "class" "col-md-6" <<< (incBtn <|> f <|> decBtn)) ) 
      `validate` (\r -> return $ if isNothing $ validation r then Nothing else Just $ b (toJSString $ fromJust $ validation r))
      where
        f = inputInt (Just initial) ! atr "size" "2" `fire` OnKeyUp
        incBtn = cbutton (initial + 1) "+" `fire` OnClick
        decBtn = cbutton (initial - 1) "-" `fire` OnClick

    makeCounter :: Int -> JSString -> String -> Widget Int
    makeCounter initial labelStr errmsg = makeCounter' initial labelStr validate
      where validate r = if r > 0 then Nothing else Just errmsg

    digitsCountCnt :: Widget Input
    digitsCountCnt = do
      newVal <- makeCounter (inputDigitsCount input) "Число битов: " "число битов должно быть положительно"
      return $ input {
        inputDigitsCount = newVal
      }

    digitsPreDotCountCnt :: Widget Input 
    digitsPreDotCountCnt = do
      newVal <- makeCounter' (inputDigitsPrevDot input) "Число битов до запятой: " 
        (\r -> if r > 0 && r <= inputDigitsCount input then Nothing else Just "должно >= 0 и <= общего числа битов") 
      return $ input {
        inputDigitsPrevDot = newVal
      } 

    expectedCnt :: Widget Input 
    expectedCnt = do
      newVal <- bsrow ! atr "style" "margin-top:10px" <<< 
        ((div ! atr "class" "col-md-6" $ label ("Ожидаемое значение:" :: JSString)) ++>
         (div ! atr "class" "col-md-6" <<< f)) 
         <** (bsrow . (div ! atr "class" "col-md-6") <<< inputSubmit "Обновить" `fire` OnClick)
      return $ input {
        inputExpected = newVal
      } 
      where f = inputDouble (Just $ inputExpected input)

    fitnessCntl :: Widget Input
    fitnessCntl = do
      newFitness <- bsrow <<< (
        (bsrow $ label ("Фитнес функция: " :: JSString) ! atr "style" "margin-top: 40px; font-size: 20px") ++>
        (bsrow <<< textArea (inputFitness input) ! atr "rows" "6" ! atr "cols" "60" <++ br 
          <** (inputSubmit "Обновить" `fire` OnClick <! [atr "style" "margin-bottom: 40px"])) <++
        (bsrow $ panel "Пояснения к параметрам:" $ mconcat [
            labelRow 2 "x:" "число с плавающей запятой, аргумент функции"
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
        evolOptionsCnt' = GeneticOptions <$> mutChanceCnt <*> elitePartCnt <*> maxGenCnt <*> popCountCnt <*> indCountCnt <*> targetFitnessCnt
          <** (inputSubmit "Обновить" `fire` OnClick)

        mutChanceCnt :: Widget Double
        mutChanceCnt = bsrow <<< (
          (div ! atr "class" "col-md-6" $ label ("Шанс мутации: " :: JSString)) ++>
          (div ! atr "class" "col-md-6" <<< inputDouble (Just $ mutationChance options)
          `validate`
          (\c -> return $ if c >= 0.0 && c <= 1.0 then Nothing else Just $ b ("вероятность некорректна [0, 1]" :: JSString))))

        elitePartCnt :: Widget Double
        elitePartCnt = bsrow <<< (
          (div ! atr "class" "col-md-6" $ label ("Часть элиты: " :: JSString)) ++>
          (div ! atr "class" "col-md-6" <<< inputDouble (Just $ elitePart options)
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

        targetFitnessCnt :: Widget (Maybe Double)
        targetFitnessCnt = return . Just =<< bsrow <<< (
          (div ! atr "class" "col-md-6" $ label ("Ожидаемый фитнес: " :: JSString)) ++>
          (div ! atr "class" "col-md-6" <<< inputDouble (targetFitness options)))
