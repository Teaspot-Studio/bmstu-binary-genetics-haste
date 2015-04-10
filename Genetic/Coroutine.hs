module Genetic.Coroutine where

import Control.Monad.Trans.Class
import Control.Monad.Random
import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors
import HasteExt.Random

type GenRand = RandT HasteGen 
type Pauseable = Coroutine (Yield ()) IO
type PauseableRand = GenRand Pauseable

pause :: PauseableRand ()
pause = lift $ yield ()
