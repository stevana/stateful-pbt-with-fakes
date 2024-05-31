{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}

module Example.Counter where

import Control.Concurrent
import Control.Monad
import Data.IORef
import Data.Void
import System.IO.Unsafe
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Parallel
import Stateful

------------------------------------------------------------------------

gLOBAL_COUNTER :: IORef Int
gLOBAL_COUNTER = unsafePerformIO (newIORef 0)
{-# NOINLINE gLOBAL_COUNTER #-}

incr :: IO ()
incr = atomicModifyIORef' gLOBAL_COUNTER (\n -> (n + 1, ()))

incr42Bug :: IO ()
incr42Bug = atomicModifyIORef' gLOBAL_COUNTER
  (\n -> if n == 42 then (n, ()) else (n + 1, ()))

incrRaceCondition :: IO ()
incrRaceCondition = do
  n <- readIORef gLOBAL_COUNTER
  threadDelay 2000
  writeIORef gLOBAL_COUNTER (n + 1)

get :: IO Int
get = readIORef gLOBAL_COUNTER

reset :: IO ()
reset = writeIORef gLOBAL_COUNTER 0

------------------------------------------------------------------------

newtype Counter = Counter Int
  deriving (Eq, Show)

instance StateModel Counter where

  -- We start counting from zero.
  initialState :: Counter
  initialState = Counter 0

  -- The commands correspond to the names of the functions that operate on the
  -- global counter.
  data Command Counter r
    = Incr
    | Get
    deriving (Show, Functor, Foldable, Traversable)

  -- The responses correspond to the return types of each function. By
  -- convention we'll add a underscore suffix to a response of the corresponding
  -- command.
  data Response Counter r
    = Incr_ ()
    | Get_ Int
    deriving (Eq, Show, Functor, Foldable)

  -- We'll generate increments and reads of the counter with equal probability.
  generateCommand :: Counter -> Gen (Command Counter r)
  generateCommand _s = elements [Incr, Get]

  -- The fake takes a command and the model of the counter and returns a new
  -- model and a response.
  runFake :: Command Counter r -> Counter -> Either Void (Counter, Response Counter r)
  runFake Incr  (Counter n) = return (Counter (n + 1), Incr_ ())
  runFake Get m@(Counter n) = return (m, Get_ n)

  -- We also need to explain which part of the counter API each command
  -- corresponds to.
  runReal :: Command Counter r -> IO (Response Counter r)
  runReal Get  = Get_  <$> get
  -- runReal Incr = Incr_ <$> incr
  -- runReal Incr = Incr_ <$> incr42Bug
  runReal Incr = Incr_ <$> incrRaceCondition

  -- This example has no references.
  type Reference Counter = Void

  -- The command monad is IO, so we don't need to do anything here.
  runCommandMonad _s = id

prop_counter :: Commands Counter -> Property
prop_counter cmds = monadicIO $ do
  runCommands cmds
  run reset
  assert True

prop_parallelCounter :: ParallelCommands Counter -> Property
prop_parallelCounter cmds = monadicIO $ do
  replicateM_ 10 $ do
    runParallelCommands cmds
    run reset
  assert True
