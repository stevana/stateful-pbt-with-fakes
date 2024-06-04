{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Parallel where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception (SomeException, displayException, try)
import Control.Monad.IO.Class
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

class (StateModel state, Eq state) => ParallelModel state where

  -- If another command monad is used we need to provide a way run it inside the
  -- IO monad. This is only needed for parallel testing, because IO is the only
  -- monad we can execute on different threads.
  runCommandMonad :: proxy state -> CommandMonad state a -> IO a

  disjointStates :: state -> state -> Bool
  disjointStates _s1 _s2 = False

-- start snippet parallel-commands
newtype ParallelCommands state = ParallelCommands
  { unParallelCommands :: [[Command state (Var (Reference state))]]
  }
-- end snippet parallel-commands
deriving stock instance
  Show (Command state (Var (Reference state))) => Show (ParallelCommands state)
deriving stock instance
  Eq (Command state (Var (Reference state))) => Eq (ParallelCommands state)

instance ParallelModel state => Arbitrary (ParallelCommands state) where

  arbitrary :: Gen (ParallelCommands state)
  arbitrary = ParallelCommands <$> go initialState
    where
      go :: state -> Gen [[Command state (Var (Reference state))]]
      go s = sized $ \n ->
        let
          w = n `div` 2 + 1
        in
          frequency
            [ (1, return [])
            , (w, do k <- frequency [ (50, return 1) -- 50% single threaded
                                    , (30, return 2) -- 30% double threaded
                                    , (20, return 3) -- 20% triple threaded
                                    ]
                     mCmds <- vectorOf k (generateCommand s)
                                `suchThatMaybe` parallelSafe s
                     case mCmds of
                       Nothing   -> return []
                       Just cmds -> (cmds :) <$> go (nextStateParallel s cmds))
            ]

  -- TODO: Improve by: moving commands from forks into non forks and moving
  -- forks after non forks.
  shrink :: ParallelCommands state -> [ParallelCommands state]
  shrink (ParallelCommands cmdss0)
    = map (ParallelCommands . pruneParallel . map (map fst))
          (shrinkList (shrinkList shrinker) (withParStates cmdss0))
    where
      shrinker :: (Command state (Var (Reference state)), state)
               -> [(Command state (Var (Reference state)), state)]
      shrinker (cmd, s) = [ (cmd', s) | cmd' <- shrinkCommand s cmd ]

      pruneParallel :: StateModel state
                    => [[Command state (Var (Reference state))]]
                    -> [[Command state (Var (Reference state))]]
      pruneParallel = go initialState Set.empty []
        where
          go _s _vars acc [] = reverse acc
          go  s  vars acc (cmds : cmdss)
            | not (parallelSafe s cmds) = go s vars acc cmdss
            | otherwise =
                let
                  p = prune s vars cmds
                in
                  go (prunedState p) (prunedScope p) (prunedCommands p : acc) cmdss

          prune :: StateModel state
                => state -> Set (Var (Reference state))
                -> [Command state (Var (Reference state))]
                -> Pruned state
          prune s0 vars0 = go' s0 vars0 []
            where
              go' s vars acc [] = Pruned s vars (reverse acc)
              go' s vars acc (cmd : cmds)
                 | not (scopeCheck vars0 cmd) = go' s vars acc cmds
                 -- ^ NOTE: We always check scope against `vars0`, i.e. the
                 -- scope *before* any of this batch of commands which will be
                 -- executed in parallel. If we'd used `vars`, then we would
                 -- allow two parallel commands to affect each other's scope,
                 -- which can lead to scope errors in some interleavings.
                 | otherwise = case runFake cmd s of
                    Left _preconditionFailure ->
                      error "prune, impossible: already checked in parallelSafe"
                    Right (s', resp) ->
                      let
                        returnedVars = Set.fromList (toList resp)
                        vars' = returnedVars `Set.union` vars
                      in
                        go' s' vars' (cmd : acc) cmds

parallelSafe :: ParallelModel state
             => state -> [Command state (Var (Reference state))] -> Bool
parallelSafe s cmds0 = preconditionsHold && endUpInSameOrDisjointStates
  where
     preconditionsHold = all (go s) (permutations cmds0)
       where
         go _s' [] = True
         go  s' (cmd : cmds)
           | precondition s' cmd = go (nextState s' cmd) cmds
           | otherwise           = False

     endUpInSameOrDisjointStates = and
       [ nextState s l == nextState s r ||
         disjointStates (nextState s l) (nextState s r)
       | (l, r) <- pairwise cmds0
       ]

pairwise :: [a] -> [(a, a)]
pairwise []       = []
pairwise (x : xs) = [ (x, y) | y <- xs ] ++ pairwise xs

withParStates :: StateModel state
              => [[Command state (Var (Reference state))]]
              -> [[(Command state (Var (Reference state)), state)]]
withParStates = go initialState
  where
    go _s []             = []
    go  s (cmds : cmdss) =
      let
        (s', cmdsAndStates) = withStates s cmds
      in
        cmdsAndStates : go s' cmdss

nextStateParallel :: StateModel state
                  => state -> [Command state (Var (Reference state))] -> state
nextStateParallel s cmds = foldl' nextState s cmds

------------------------------------------------------------------------

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

toPid :: ThreadId -> Pid
toPid tid = Pid (read (drop (length ("ThreadId " :: String)) (show tid)))

data Op state = Op (Command state (Var (Reference state)))
                   (Response state (Reference state))

------------------------------------------------------------------------

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

linearisable :: forall state. StateModel state
             => Env state -> Forest (Op state) -> Bool
linearisable env = any' (go initialState)
  where
    go :: state -> Tree (Op state) -> Bool
    go s (Node (Op cmd cresp) ts) =
      case runFake cmd s of
        Left err ->
          error $ "linearisable: impossible, all precondtions are satisifed during generation\ncmd = " ++
                  show cmd ++ "\ns = " ++ show s ++ "\nerr = " ++ show err
        Right (s', resp) ->
          cresp == fmap (lookupEnv env) resp && any' (go s') ts

    any' :: (a -> Bool) -> [a] -> Bool
    any' _p [] = True
    any'  p xs = any p xs

------------------------------------------------------------------------

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

------------------------------------------------------------------------

runParallelCommands :: forall state. ParallelModel state
                    => ParallelCommands state -> PropertyM IO ()
runParallelCommands (ParallelCommands cmdss0) = do
  forM_ (concat cmdss0) $ \cmd ->
    let name = commandName cmd in
      monitor (tabulate "Commands" [name] . classify True name)
  monitor (tabulate "Concurrency" (map (show . length) cmdss0))
  evs <- liftIO newTQueueIO
  c <- liftIO newAtomicCounter
  env <- go evs c emptyEnv cmdss0
  hist <- History <$> liftIO (atomically (flushTQueue evs))
  monitor (counterexample (show hist))
  assert (linearisable env (interleavings hist))
  where
    go _evs _c env [] = return env
    go  evs  c env (cmds : cmdss) = do
      envs <- liftIO $
        mapConcurrently (runParallelReal evs c env) cmds
      let env' = combineEnvs (env : envs)
      go evs c env' cmdss

runParallelReal :: forall state. ParallelModel state
                => TQueue (Event state)
                -> AtomicCounter
                -> Env state
                -> Command state (Var (Reference state))
                -> IO (Env state)
runParallelReal evs c env cmd = do
  pid <- toPid <$> liftIO myThreadId
  liftIO (atomically (writeTQueue evs (Invoke pid cmd)))
  eResp <- try (runCommandMonad (Proxy :: Proxy state) (runReal (fmap (lookupEnv env) cmd)))
  case eResp of
    Left (err :: SomeException) ->
      error ("runParallelReal: " ++ displayException err)
    Right resp -> do
      -- NOTE: It's important that we extend the environment before writing `Ok`
      -- to the history, otherwise we might get scope issues.
      env' <- extendEnvParallel env c (toList resp)
      liftIO (atomically (writeTQueue evs (Ok pid resp)))
      return env'
