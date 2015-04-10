{-# LANGUAGE OverloadedStrings #-}
module Binary.Application where

import Prelude hiding (div)
import Data.Monoid
import Data.Maybe
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors
import Control.Applicative
import Binary.Field
import Binary.Task
import Binary.Util
import Binary.Genetic
import Binary.Plot
import Binary.Config
import Binary.Tower
import System.Random
import Haste.HPlay.View hiding (head)
import Haste
import Genetic.Options
import Genetic.Solve 
import Genetic.State
import Genetic.Coroutine

data ApplicationState = AppConfigure Input 
  | AppCalculate Input PlotState (GeneticState TowersIndivid) (Maybe (Pauseable (GeneticState TowersIndivid)))
  | AppShow Input PlotState Output

data Route = RouteConfig | RouteCalculate | RouteShow
  deriving (Enum, Show)

initialState :: ApplicationState
initialState = AppConfigure initialInput

runApplication :: ApplicationState -> Widget ()
runApplication state = wloop state go
  where 
    go :: ApplicationState -> Widget ApplicationState
    go localState@(AppConfigure input) = do
      update <- eitherWidget (fieldConfigWidget input) $ routeWidget localState
      case update of
        Right route -> case route of 
          RouteCalculate -> do
            geneticState <- liftIO initialGeneticState
            return $ AppCalculate input initialPlotState geneticState Nothing
          _ -> fail $ "invalid route in config state " ++ show route
        Left newInput -> return $ AppConfigure newInput

    go localState@(AppCalculate input plotState geneticState coroutine) = do
      update <- eitherWidget (geneticWidget input geneticState plotState coroutine) $ routeWidget localState
      case update of 
        Right route -> do
          liftIO $ clearTimers
          case route of 
            RouteConfig -> return $ AppConfigure input 
            RouteShow -> return $ AppShow input plotState $ extractSolution input geneticState
            _ -> fail $ "invalid route in config state " ++ show route
        Left (newGeneticState, newPlotState, newCoroutine) -> return $ if isGeneticFinished newGeneticState 
          then AppShow input newPlotState $ extractSolution input newGeneticState
          else AppCalculate input newPlotState newGeneticState newCoroutine

    go localState@(AppShow input plotState output) = do
      update <- eitherWidget (showResultsWidget input plotState output) $ routeWidget localState
      case update of
        Right route -> case route of 
          RouteConfig -> return $ AppConfigure input 
          RouteCalculate -> do 
            geneticState <- liftIO initialGeneticState
            return $ AppCalculate input initialPlotState geneticState Nothing
          _ -> fail $ "invalid route in show state " ++ show route
        Left _ -> return localState

eitherWidget :: Widget a -> Widget b -> Widget (Either a b)
eitherWidget wa wb = (return . Left =<< wa) <|> (return . Right =<< wb)

routeWidget :: ApplicationState -> Widget Route
routeWidget state = div ! atr "class" "row" 
  <<< div ! atr "class" "col-md-4 col-md-offset-4"
  <<< go state
  where
    go (AppConfigure {}) = bigBtn RouteCalculate "Начать эволюцию"
    go (AppCalculate {}) = bigBtn RouteConfig "Назад"  <|> bigBtn RouteShow "Остановить"
    go (AppShow {}) = bigBtn RouteConfig "Начать с начала" <|> bigBtn RouteCalculate "Перерасчитать"

    bigBtn v s = cbutton v s <! [atr "class" "btn btn-primary btn-lg"]

geneticWidget :: Input -> GeneticState TowersIndivid -> PlotState -> Maybe (Pauseable (GeneticState TowersIndivid)) -> Widget (GeneticState TowersIndivid, PlotState, Maybe (Pauseable (GeneticState TowersIndivid)))
geneticWidget input geneticState plotState coroutine = do 
  --wprint $ show $ geneticCurrentBest geneticState

  let newPlotState =  if null $ geneticPopulations geneticState
                      then plotState
                      else plotState 
                      { 
                        values = values plotState ++ [
                          ( geneticCurrentGen geneticState, 
                            fromMaybe 0 $ fst <$> geneticCurrentBest geneticState
                          )] 
                      }

  (dwidth, dheight) <- liftIO $ getDocumentSize
  div ! atr "class" "col-md-6" <<< plotWidget newPlotState "Поколение" "Фитнес" ( 0.4 * fromIntegral dwidth, fromIntegral dheight / 2)
  let towersUsed = length $ maybe [] (filterTowers input) $ snd <$> geneticCurrentBest geneticState
  wraw $ div ! atr "class" "col-md-6" $ panel "Текущий результат" $ mconcat [
      labelRow 4 "Лучший фитнес: " $ show $ maybe 0 fst $ geneticCurrentBest geneticState
    , labelRow 4 "Башен использовано: " $ show $ towersUsed
    , labelRow 4 "Башен всего: " $ show $ length $ inputTowers input
    , labelRow 4 "Лучшее покрытие: " $ maybe "" show $ calcCoverage input . snd <$> geneticCurrentBest geneticState
    ]

  corRes <- timeout 100 $ liftIO $ case coroutine of 
    Nothing -> resume $ solve (length $ inputTowers input) (fitness input) (inputGeneticOptions input) geneticState
    Just cr -> resume cr
  (newGeneticState, newCoroutine) <- case corRes of 
    Left (Yield _ paused) -> return (geneticState, Just paused)
    Right genst -> return (genst, Nothing)
  
  return (newGeneticState, newPlotState, newCoroutine)

showResultsWidget :: Input -> PlotState -> Output -> Widget ()
showResultsWidget input plotState output = do
  (dwidth, dheight) <- liftIO $ getDocumentSize
  let (xsize, ysize) = inputFieldSize input
      cellSize = fromIntegral dwidth * 0.45 / fromIntegral xsize
  div ! atr "class" "row" <<< do
    div ! atr "class" "col-md-6" <<< fieldShow input output cellSize
    div ! atr "class" "col-md-6" <<< do
      plotWidget plotState "Поколение" "Фитнес" (fromIntegral dwidth / 2, fromIntegral dheight / 2)
      wraw $ div ! atr "class" "row-fluid" $ mconcat [
          div ! atr "class" "col-md-6" $ inputInfo
        , div ! atr "class" "col-md-6" $ optionsInfo
        , div ! atr "class" "col-md-6" $ outputInfo
        , div ! atr "class" "col-md-6" $ otherInfo
        ]
    noWidget
  where
    opts = inputGeneticOptions input 
    showTower t = "x: " ++ show (towerX t) ++ " y: " ++ show (towerY t) ++ " r: " ++ show (towerRadius t)
    showTower' t = "x: " ++ show (towerX t) ++ " y: " ++ show (towerY t)

    inputInfo = panel "Входные данные" $ mconcat [
        labelRow 4 "Размер поля:" $ show $ inputFieldSize input
      , labelRow 4 "Возможные башни:" $ show $ showTower <$> inputTowers input
      ]

    optionsInfo = panel "Настройки эволюции" $ mconcat [
        labelRow 4 "Шанс мутации: " $ show $ mutationChance opts
      , labelRow 4 "Часть элиты: " $ show $ elitePart opts
      , labelRow 4 "Максимальное число поколений: " $ show $ maxGeneration opts
      , labelRow 4 "Кол-во популяций: " $ show $ popCount opts
      , labelRow 4 "Кол-во индивидов в популяции: " $ show $ indCount opts
      ]

    outputInfo = panel "Результаты эволюции" $ mconcat [
        labelRow 4 "Лучший фитнес: " $ show $ outputFitness output
      , labelRow 4 "Лучшее решение: " $ show $ showTower' <$> outputTowers output
      ]

    otherInfo = panel "Другая информация" $ mconcat [
        labelRow 4 "Башен использовано: " $ show $ length $ outputTowers output
      , labelRow 4 "Башен всего: " $ show $ length $ inputTowers input
      , labelRow 4"Лучшее покрытие: " $ show $ calcCoverage' input $ outputTowers output
      ]