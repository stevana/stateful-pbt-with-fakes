{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Parallel where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception (SomeException, displayException, try)
import Control.Monad.IO.Class
import Data.Coerce
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable
import Data.IORef
import Data.List (permutations)
import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tree
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Stateful

------------------------------------------------------------------------

-- start snippet ParallelModel
class (StateModel state, Ord state) => ParallelModel state where

  -- If another command monad is used we need to provide a way run it inside the
  -- IO monad. This is only needed for parallel testing, because IO is the only
  -- monad we can execute on different threads.
  runCommandMonad :: proxy state -> CommandMonad state a -> IO a

  generateCommandParallel :: [state] -> Gen (Command state (Var (Reference state)))
  generateCommandParallel ss = do
    s <- elements ss
    generateCommand s

  shrinkCommandParallel :: [state] -> Command state (Var (Reference state))
                        -> [Command state (Var (Reference state))]
  shrinkCommandParallel ss cmd = shrinkCommand (maximum ss) cmd
-- end snippet ParallelModel

-- start snippet ParallelCommands
newtype ParallelCommands state = ParallelCommands [Fork state]

newtype Fork state = Fork [Command state (Var (Reference state))]
-- end snippet ParallelCommands

unParallelCommands :: ParallelCommands state -> [Fork state]
unParallelCommands (ParallelCommands forks) = forks

unFork :: Fork state -> [Command state (Var (Reference state))]
unFork (Fork cmds) = cmds

deriving stock instance
  Show (Command state (Var (Reference state))) => Show (ParallelCommands state)
deriving stock instance
  Eq (Command state (Var (Reference state))) => Eq (ParallelCommands state)

deriving stock instance
  Show (Command state (Var (Reference state))) => Show (Fork state)
deriving stock instance
  Eq (Command state (Var (Reference state))) => Eq (Fork state)

parallelCommands :: forall state. ParallelCommands state -> [Command state (Var (Reference state))]
parallelCommands = concat . coerce @_ @[[Command state (Var (Reference state))]]

instance ParallelModel state => Arbitrary (ParallelCommands state) where

  -- start snippet arbitrary
  arbitrary :: Gen (ParallelCommands state)
  arbitrary = ParallelCommands <$> go [initialState]
    where
      go :: [state] -> Gen [Fork state]
      go ss = sized $ \n ->
        let
          w = n `div` 2 + 1
        in
          frequency
            [ (1, return [])
            , (w, do k <- frequency [ (50, return 1) -- 50% single threaded
                                    , (30, return 2) -- 30% double threaded
                                    , (20, return 3) -- 20% triple threaded
                                    ]
                     mCmds <- vectorOf k (generateCommandParallel ss)
                                `suchThatMaybe` (parallelSafe ss . Fork)
                     case mCmds of
                       Nothing   -> return []
                       Just cmds ->
                         (Fork cmds :) <$> go (nextStates ss cmds))
            ]
  -- end snippet arbitrary

  -- TODO: Improve by: moving commands from forks into non forks and moving
  -- forks after non forks.
  -- start snippet shrink
  shrink :: ParallelCommands state -> [ParallelCommands state]
  shrink = pruneShrinks . possibleShrinks
    where
      possibleShrinks :: ParallelCommands state -> [ParallelCommands state]
      possibleShrinks
        = map (coerce . map (map fst))
        . shrinkList (shrinkList shrinker) . withParStates . unParallelCommands
        where
          withParStates :: (StateModel state, Ord state)
                        => [Fork state]
                        -> [[(Command state (Var (Reference state)), [state])]]
          withParStates = go [initialState] [] . coerce
            where
              go _ss acc []             = reverse acc
              go  ss acc (cmds : cmdss) =
                go (nextStates ss cmds) (map (\cmd -> (cmd, ss)) cmds : acc) cmdss

          shrinker :: (Command state (Var (Reference state)), [state])
                   -> [(Command state (Var (Reference state)), [state])]
          shrinker (cmd, ss) = [ (cmd', ss) | cmd' <- shrinkCommandParallel ss cmd ]

      pruneShrinks :: [ParallelCommands state] -> [ParallelCommands state]
      pruneShrinks = coerce . filter (not . null)
                   . map (go [initialState] Set.empty [] . unParallelCommands)
        where
          go :: [state] -> Set (Var (Reference state)) -> [Fork state] -> [Fork state] -> [Fork state]
          go _ss _vars acc [] = reverse acc
          go  ss  vars acc (fork@(Fork cmds) : forks)
            | all (scopeCheck vars) cmds
            , parallelSafe ss fork =
              let
                ss'   = nextStates ss cmds
                vars' = getReturnedVars (head ss) vars cmds -- NOTE: head is safe
              in
                go ss' vars' (fork : acc) forks
            | otherwise            = go ss vars acc forks

          -- It doesn't matter which of the possible states we start in, as all
          -- commands in a fork pass their preconditions in all states. It also
          -- doesn't matter in which interleaving we gather the responses, as
          -- all we do is collect the `Var`s that get returned into an unordered
          -- `Set`.
          getReturnedVars _s vars [] = vars
          getReturnedVars s vars (cmd : cmds) = case runFake cmd s of
            Left _preconditionFailed ->
              error "getReturnedVars: impossible, parallelSafe checks that all preconditions hold"
            Right (_s', resp) ->
              getReturnedVars s (vars `Set.union` Set.fromList (toList resp)) cmds
  -- end snippet shrink

-- start snippet parallelSafe
parallelSafe :: ParallelModel state => [state] -> Fork state -> Bool
parallelSafe ss (Fork cmds0) = and
  [ preconditionsHold s cmds | s <- toList ss, cmds <- permutations cmds0 ]
  where
    preconditionsHold s0 = all (go s0) . permutations
      where
        go _s [] = True
        go  s (cmd : cmds)
          | precondition s cmd = go (nextState s cmd) cmds
          | otherwise          = False
-- end snippet parallelSafe

-- start snippet nextStates
nextStates :: (StateModel state, Ord state)
           => [state] -> [Command state (Var (Reference state))] -> [state]
nextStates ss cmds = nubOrd [ foldl' nextState s cmds | s <- ss ]
-- end snippet nextStates

------------------------------------------------------------------------

-- start snippet History
newtype History state = History [Event state]
deriving stock instance
   (Show (Command state (Var (Reference state))),
    Show (Response state (Reference state))) => Show (History state)

data Event state
  = Invoke Pid (Command state (Var (Reference state)))
  | Ok     Pid (Response state (Reference state))
deriving stock instance
  (Show (Command state (Var (Reference state))),
   Show (Response state (Reference state))) => Show (Event state)

newtype Pid = Pid Int
  deriving stock (Eq, Ord, Show)
  deriving newtype Enum
-- end snippet History

------------------------------------------------------------------------

-- start snippet interleavings
data Op state = Op (Command state (Var (Reference state)))
                   (Response state (Reference state))

interleavings :: History state -> Forest (Op state)
interleavings (History [])  = []
interleavings (History evs0) =
  [ Node (Op cmd resp) (interleavings (History evs'))
  | (tid, cmd)   <- takeInvocations evs0
  , (resp, evs') <- findResponse tid
                      (filter1 (not . matchInvocation tid) evs0)
  ]
  where
    takeInvocations :: [Event state] -> [(Pid, Command state (Var (Reference state)))]
    takeInvocations []                         = []
    takeInvocations ((Invoke pid cmd)   : evs) = (pid, cmd) : takeInvocations evs
    takeInvocations ((Ok    _pid _resp) : _)   = []

    findResponse :: Pid -> [Event state] -> [(Response state (Reference state), [Event state])]
    findResponse _pid []                                   = []
    findResponse  pid ((Ok pid' resp) : evs) | pid == pid' = [(resp, evs)]
    findResponse  pid (ev             : evs)               =
      [ (resp, ev : evs') | (resp, evs') <- findResponse pid evs ]

    matchInvocation :: Pid -> Event state -> Bool
    matchInvocation pid (Invoke pid' _cmd) = pid == pid'
    matchInvocation _   _                  = False

    filter1 :: (a -> Bool) -> [a] -> [a]
    filter1 _ []                   = []
    filter1 p (x : xs) | p x       = x : filter1 p xs
                       | otherwise = xs
-- end snippet interleavings

-- start snippet linearisable
linearisable :: forall state. StateModel state
             => Env state -> Forest (Op state) -> Bool
linearisable env = any' (go initialState)
  where
    go :: state -> Tree (Op state) -> Bool
    go s (Node (Op cmd cresp) ts) =
      case runFake cmd s of
        Left _preconditionFailure ->
          error "linearisable: impossible, all precondtions are satisifed during generation"
        Right (s', resp) ->
          cresp == fmap (lookupEnv env) resp && any' (go s') ts

    any' :: (a -> Bool) -> [a] -> Bool
    any' _p [] = True
    any'  p xs = any p xs
-- end snippet linearisable

------------------------------------------------------------------------

-- start snippet env
newtype AtomicCounter = AtomicCounter (IORef Int)

newAtomicCounter :: IO AtomicCounter
newAtomicCounter = AtomicCounter <$> newIORef 0

-- Returns old value.
incrAtomicCounter :: AtomicCounter -> Int -> IO Int
incrAtomicCounter (AtomicCounter ioRef) n =
  atomicModifyIORef' ioRef (\old -> (old + n, old))

extendEnvParallel :: Env state -> AtomicCounter -> [Reference state] -> IO (Env state)
extendEnvParallel env c refs = do
  i <- incrAtomicCounter c (length refs)
  return (extendEnv env (zip [i..] refs))
-- end snippet env

------------------------------------------------------------------------

-- start snippet runParallelCommands
runParallelCommands :: forall state. ParallelModel state
                    => ParallelCommands state -> PropertyM IO ()
runParallelCommands cmds0@(ParallelCommands forks0) = do
  forM_ (parallelCommands cmds0) $ \cmd -> do
    let name = commandName cmd
    monitor (tabulate "Commands" [name] . classify True name)
  monitor (tabulate "Concurrency" (map (show . length . unFork) forks0))
  q   <- liftIO newTQueueIO
  c   <- liftIO newAtomicCounter
  env <- liftIO (runForks q c emptyEnv forks0)
  hist <- History <$> liftIO (atomically (flushTQueue q))
  monitor (counterexample (show hist))
  assert (linearisable env (interleavings hist))
  where
    runForks :: TQueue (Event state) -> AtomicCounter -> Env state -> [Fork state]
             -> IO (Env state)
    runForks _q _c env [] = return env
    runForks  q  c env (Fork cmds : forks) = do
      envs <- liftIO $
        mapConcurrently (runParallelReal q c env) (zip [Pid 0..] cmds)
      let env' = combineEnvs (env : envs)
      runForks q c env' forks

    runParallelReal :: TQueue (Event state) -> AtomicCounter -> Env state
                    -> (Pid, Command state (Var (Reference state))) -> IO (Env state)
    runParallelReal q c env (pid, cmd) = do
      atomically (writeTQueue q (Invoke pid cmd))
      eResp <- try (runCommandMonad (Proxy :: Proxy state) (runReal (fmap (lookupEnv env) cmd)))
      case eResp of
        Left (err :: SomeException) ->
          error ("runParallelReal: " ++ displayException err)
        Right resp -> do
          -- NOTE: It's important that we extend the environment before writing `Ok`
          -- to the history, otherwise we might get scope issues.

          -- XXX: Move outside of mapConcurrently? How do we assign the right `Var`
          -- with each `Reference`? Perhaps this would be easier if we had a prefix
          -- and N suffixes?
          env' <- extendEnvParallel env c (toList resp)
          atomically (writeTQueue q (Ok pid resp))
          return env'
-- end snippet runParallelCommands
