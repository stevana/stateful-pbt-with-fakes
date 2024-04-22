{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Parallel where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception (SomeException, try, displayException)
import Control.Monad.IO.Class
import Data.Dynamic
import Data.Foldable
import Data.List
import Data.Proxy
import Data.Tree
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Stateful

------------------------------------------------------------------------

newtype ParallelCommands state = ParallelCommands
  { unParallelCommands :: [[Command state (Var (Reference state))]]
  }
deriving stock instance
  Show (Command state (Var (Reference state))) => Show (ParallelCommands state)
deriving stock instance
  Eq (Command state (Var (Reference state))) => Eq (ParallelCommands state)

instance StateModel state => Arbitrary (ParallelCommands state) where

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
            , (w, do k <- chooseInt (1, 5)
                     mcmds <- vectorOf k (generateCommand s)
                                `suchThatMaybe` parSafe s
                     case mcmds of
                       Nothing   -> return []
                       Just cmds -> (cmds :) <$> go (nextStateParallel s cmds))
            ]

  shrink :: ParallelCommands state -> [ParallelCommands state]
  shrink (ParallelCommands cmdss0)
    = map (ParallelCommands . pruneParallel . map (map fst))
          (shrinkList (shrinkList shrinker) (map withStates cmdss0))
    where
      shrinker :: (Command state (Var (Reference state)), state)
               -> [(Command state (Var (Reference state)), state)]
      shrinker (cmd, s) = [ (cmd', s) | cmd' <- shrinkCommand s cmd ]

      pruneParallel :: StateModel state
                    => [[Command state (Var (Reference state))]]
                    -> [[Command state (Var (Reference state))]]
      pruneParallel = go initialState
        where
          go _s [] = []
          go s (cmds : cmdss)
            | parSafe s cmds = cmds : go (nextStateParallel s cmds) cmdss
            | otherwise      =        go s cmdss

parSafe :: StateModel state
        => state -> [Command state (Var (Reference state))] -> Bool
parSafe s0 = all (validCommands s0) . permutations
  where
    validCommands :: StateModel state
                  => state -> [Command state (Var (Reference state))] -> Bool
    validCommands _s []           = True
    validCommands s  (cmd : cmds)
      | precondition s cmd = validCommands (nextState s cmd) cmds
      | otherwise          = False

nextStateParallel :: StateModel state
                  => state -> [Command state (Var (Reference state))] -> state
nextStateParallel s cmds = foldl' (\ih cmd -> nextState ih cmd) s cmds

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

linearisable :: forall state. StateModel state => [(Int, Dynamic)] -> Forest (Op state) -> Bool
linearisable vars = any' (go initialState)
  where
    go :: state -> Tree (Op state) -> Bool
    go s (Node (Op cmd cresp) ts) =
      case runFake cmd s of
        Left _err -> error "linearisable: impossible, all precondtions are satisifed during generation"
        Right (s', resp) ->
          cresp == fmap (sub vars) resp && any' (go s') ts

    any' :: (a -> Bool) -> [a] -> Bool
    any' _p [] = True
    any'  p xs = any p xs

------------------------------------------------------------------------

runParallelCommands :: forall state. StateModel state
                    => ParallelCommands state -> PropertyM (CommandMonad state) ()
runParallelCommands (ParallelCommands cmdss0) = do
  forM_ (concat cmdss0) $ \cmd ->
    let name = commandName cmd in
      monitor (tabulate "Commands" [name] . classify True name)
  monitor (tabulate "Number of concurrent commands" (map (show . length) cmdss0))
  evs <- liftIO newTQueueIO :: PropertyM (CommandMonad state) (TQueue (Event state))
  vars <- go evs [] cmdss0
  hist <- History <$> liftIO (atomically (flushTQueue evs))
  monitor (counterexample (show hist))
  assert (linearisable vars (interleavings hist))
  where
    go _evs vars [] = return vars
    go evs vars (cmds : cmdss) = do
      refss <- liftIO $
        mapConcurrently (\cmd -> runParallelReal evs (sub vars) cmd) cmds
      let vars' = vars ++ zip [length vars..] (map toDyn (concat refss))
      go evs vars' cmdss

runParallelReal :: forall state. StateModel state
                => TQueue (Event state) -> Env state
                -> Command state (Var (Reference state))
                -> IO [Reference state]
runParallelReal evs env cmd = do
  pid <- toPid <$> liftIO myThreadId
  liftIO (atomically (writeTQueue evs (Invoke pid cmd)))
  eResp <- try (runCommandMonad (Proxy :: Proxy state) (runReal (fmap env cmd)))
  case eResp of
    Left (err :: SomeException) -> error (displayException err)
    Right resp -> do
      liftIO (atomically (writeTQueue evs (Ok pid resp)))
      return (toList resp)
