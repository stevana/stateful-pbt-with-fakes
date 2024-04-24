{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}

module Example.Counter where

import Control.Concurrent
import Data.IORef
import Data.Void
import Test.QuickCheck
import Test.QuickCheck.Monadic
import System.IO.Unsafe

import Stateful
import Parallel

------------------------------------------------------------------------

gLOBAL_COUNTER :: IORef Int
gLOBAL_COUNTER = unsafePerformIO (newIORef 0)
{-# NOINLINE gLOBAL_COUNTER #-}

data Bug = NoBug | Incr42Bug | RaceCondition

bUG :: Bug
bUG = RaceCondition

------------------------------------------------------------------------

data Model = Model Int
  deriving (Eq, Show)

instance StateModel Model where

  initialState = Model 0

  type Reference Model = Void

  data Command Model r
    = Incr
    | Read
    deriving (Show, Functor)

  data Response Model r
    = Incr_ ()
    | Read_ Int
    deriving (Eq, Show, Functor, Foldable)

  generateCommand :: Model -> Gen (Command Model r)
  generateCommand _s = elements [Incr, Read]

  runFake :: Command Model r -> Model -> Either Void (Model, Response Model r)
  runFake Incr   (Model n) = return (Model (n + 1), Incr_ ())
  runFake Read m@(Model n) = return (m, Read_ n)

  runReal :: Command Model r -> IO (Response Model r)
  runReal Read = Read_ <$> readIORef    gLOBAL_COUNTER
  runReal Incr = Incr_ <$> incr
    where
      incr = case bUG of
        NoBug     -> atomicModifyIORef' gLOBAL_COUNTER (\n -> (n + 1, ()))
        Incr42Bug -> modifyIORef' gLOBAL_COUNTER
                       (\n -> if n == 42 then n else n + 1)
        RaceCondition -> do
          n <- readIORef gLOBAL_COUNTER
          threadDelay 2000
          writeIORef gLOBAL_COUNTER (n + 1)

  runCommandMonad _s = id

prop_counter :: Commands Model -> Property
prop_counter cmds = monadicIO $ do
  runCommands cmds
  resetCounter
  assert True

resetCounter :: PropertyM IO ()
resetCounter = run (writeIORef gLOBAL_COUNTER 0)

prop_parallelCounter :: ParallelCommands Model -> Property
prop_parallelCounter cmds = monadicIO $ do
  runParallelCommands cmds
  resetCounter
  assert True
