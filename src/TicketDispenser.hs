{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module TicketDispenser where

import Control.Monad
import Control.Concurrent
import Data.IORef
import Test.QuickCheck
import Test.QuickCheck.Monadic

import StateModel
import Parallel

------------------------------------------------------------------------

newtype Opaque a = Opaque { unOpaque :: a }

instance Show (Opaque a) where
  show _ = "<opaque>"

data State = NoState | State Int

instance StateModel State where

  initialState = NoState

  data Command State ref resp where
    New        :: Command State ref ref
    TakeTicket :: ref -> Command State ref Int
    Reset      :: ref -> Command State ref ()

  generateCommand :: State -> Gen (Untyped (Command State (Var (Reference State))))
  generateCommand NoState   = return (Untyped New)
  generateCommand (State _) = frequency
    [ (1, Untyped <$> return (Reset (Var 0)))
    , (9, Untyped <$> return (TakeTicket (Var 0)))
    ]

  type Reference State = Opaque (IORef Int)

  type Failure State = ()

  runCommandMonad _ = id

  runFake :: Command State (Var (Reference State)) resp -> State -> Either () (State, resp)
  runFake New               NoState    = return (State 0, Var 0)
  runFake New               (State _)  = Left ()
  runFake (TakeTicket _ref) (State n)  = return (State (n + 1), n)
  runFake (TakeTicket _ref) NoState    = Left ()
  runFake (Reset _ref)      (State _n) = return (State 0, ())
  runFake (Reset _ref)      NoState    = Left ()

  runReal :: Env State -> Command State (Var (Reference State)) resp -> IO (Return State resp)
  runReal _env New = Reference . Opaque <$> newIORef 0
  runReal env (TakeTicket ref) = do
    n <- readIORef (unOpaque (env ref))
    threadDelay 500
    writeIORef (unOpaque (env ref)) (n + 1)
    -- n <- atomicModifyIORef' (unOpaque (env ref)) $ \n -> (n + 1, n)
    return (Response n)
  runReal env (Reset ref) = Response <$> writeIORef (unOpaque (env ref)) 0

deriving instance Show ref => Show (Command State ref resp)

prop_seq :: Commands State -> Property
prop_seq cmds = monadicIO $ do
  _ <- runCommands cmds
  assert True

prop_par :: ParallelCommands State -> Property
prop_par cmds = monadicIO $ do
  replicateM_ 10 $
    runParallelCommands cmds
  assert True

test :: IO ()
test = verboseCheck prop_par
