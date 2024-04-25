{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Example.TicketDispenser where

import Control.Monad
import Control.Concurrent
import Data.IORef
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Stateful
import Parallel

------------------------------------------------------------------------

newtype Opaque a = Opaque { unOpaque :: a }
  deriving Eq

instance Show (Opaque a) where
  show _ = "<opaque>"

data State = NoState | State Int
  deriving Show

instance StateModel State where

  initialState = NoState

  type Reference State = Opaque (IORef Int)

  data Command State ref
    = New
    | TakeTicket ref
    | Reset ref
    deriving (Eq, Show, Functor)

  data Response State ref
    = New_ ref
    | TakeTicket_ Int
    | Reset_ ()
    deriving (Eq, Show, Functor, Foldable)

  type PreconditionFailure State = ()

  generateCommand :: State -> Gen (Command State (Var (Reference State)))
  generateCommand NoState   = return New
  generateCommand (State _) = frequency
    [ (1, return (Reset (Var 0)))
    , (9, return (TakeTicket (Var 0)))
    ]

  runCommandMonad _ = id

  runFake :: Command State (Var (Reference State)) -> State
          -> Either () (State, Response State (Var (Reference State)))
  runFake New               NoState    = return (State 0, New_ (Var 0))
  runFake New               (State _)  = Left ()
  runFake (TakeTicket _ref) (State n)  = return (State (n + 1), TakeTicket_ n)
  runFake (TakeTicket _ref) NoState    = Left ()
  runFake (Reset _ref)      (State _n) = return (State 0, Reset_ ())
  runFake (Reset _ref)      NoState    = Left ()

  runReal :: Command State (Reference State) -> IO (Response State (Reference State))
  runReal New = New_ . Opaque <$> newIORef 0
  runReal (TakeTicket ref) = do
    n <- readIORef (unOpaque ref)
    threadDelay 50
    writeIORef (unOpaque ref) (n + 1)
    -- n <- atomicModifyIORef' (unOpaque ref) $ \n -> (n + 1, n)
    return (TakeTicket_ n)
  runReal (Reset ref) = Reset_ <$> writeIORef (unOpaque ref) 0


prop_ticketDispenser :: Commands State -> Property
prop_ticketDispenser cmds = monadicIO $ do
  runCommands cmds
  assert True

prop_parallelTicketDispenser :: ParallelCommands State -> Property
prop_parallelTicketDispenser cmds = monadicIO $ do
  replicateM_ 10 $
    runParallelCommands cmds
  assert True
