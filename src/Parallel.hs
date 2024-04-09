{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Parallel where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Dynamic
import Data.List
import Data.Maybe
import Data.Tree
import Test.QuickCheck
import Test.QuickCheck.Monadic

import StateModel hiding (Event, History)

------------------------------------------------------------------------

newtype ParallelCommands state = ParallelCommands
  { unParallelCommands :: [[Untyped (Command state (Var (Reference state)))]]
  }
deriving stock instance
  (forall ref resp. Show ref => Show (Command state ref resp))
  => Show (ParallelCommands state)

instance StateModel state => Arbitrary (ParallelCommands state) where

  arbitrary :: Gen (ParallelCommands state)
  arbitrary = ParallelCommands <$> go initialState
    where
      go :: state -> Gen [[Untyped (Command state (Var (Reference state)))]]
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
                       Nothing -> return []
                       Just cmds-> (cmds :) <$> go (advanceState s cmds))
            ]

  shrink :: ParallelCommands state -> [ParallelCommands state]
  shrink (ParallelCommands cmdss)
    = map (ParallelCommands . pruneParallel . map (map fst))
          (shrinkList (shrinkList shrinker) (map withStates cmdss))
    where
      shrinker :: (Untyped (Command state (Var (Reference state))), state)
               -> [(Untyped (Command state (Var (Reference state))), state)]
      shrinker (cmd, s) = [ (cmd', s) | cmd' <- shrinkCommand s cmd ]

pruneParallel :: StateModel state
              => [[Untyped (Command state (Var (Reference state)))]]
              -> [[Untyped (Command state (Var (Reference state)))]]
pruneParallel = go initialState
  where
    go _s [] = []
    go s (cmds : cmdss)
      | parSafe s cmds = cmds : go (advanceState s cmds) cmdss
      | otherwise      =        go s cmdss

parSafe :: StateModel state => state -> [Untyped (Command state (Var (Reference state)))] -> Bool
parSafe s = all (validCommands s) . permutations

validCommands :: StateModel state => state -> [Untyped (Command state (Var (Reference state)))] -> Bool
validCommands _s []           = True
validCommands s  (cmd : cmds)
  | precondition s cmd = validCommands (nextState s cmd) cmds
  | otherwise          = False

advanceState :: StateModel state => state -> [Untyped (Command state (Var (Reference state)))] -> state
advanceState s cmds = foldl' (\ih cmd -> nextState ih cmd) s cmds

------------------------------------------------------------------------

newtype History state = History [Event state]
deriving stock instance
  (forall ref resp. Show ref => Show (Command state ref resp))
  => Show (History state)

data Event state
  = Invoke Pid (Untyped (Command state (Var (Reference state))))
  | Ok     Pid Dynamic
deriving stock instance
  (forall ref resp. Show ref => Show (Command state ref resp))
  => Show (Event state)

newtype Pid = Pid Int
  deriving stock (Eq, Ord, Show)

toPid :: ThreadId -> Pid
toPid tid = Pid (read (drop (length ("ThreadId " :: String)) (show tid)))

data Op state = forall resp. Eq resp =>
  Op (Command state (Var (Reference state)) resp)
     (Return state resp)

interleavings :: Typeable state => History state -> Forest (Op state)
interleavings (History [])  = []
interleavings (History evs0) =
  [ Node (Op cmd (fromDyn_ resp)) (interleavings (History evs'))
  | (tid, Untyped cmd) <- takeInvocations evs0
  , (resp, evs')       <- findResponse tid
                            (filter1 (not . matchInvocation tid) evs0)
  ]
  where
    fromDyn_ resp = fromDyn resp (error "interleavings: impossible")

    takeInvocations :: [Event state] -> [(Pid, Untyped (Command state (Var (Reference state))))]
    takeInvocations []                         = []
    takeInvocations ((Invoke pid cmd)   : evs) = (pid, cmd) : takeInvocations evs
    takeInvocations ((Ok    _pid _resp) : _)   = []

    findResponse :: Pid -> [Event state] -> [(Dynamic, [Event state])]
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

runParallelReal :: forall state resp. (StateModel state, MonadIO (CommandMonad state),
                    Typeable state, Typeable resp, Eq resp, Show resp)
                => TQueue (Event state) -> Env state
                -> Command state (Var (Reference state)) resp
                -> IO (Maybe (Reference state))
runParallelReal evs env cmd = do
  pid <- toPid <$> liftIO myThreadId
  liftIO (atomically (writeTQueue evs (Invoke pid (Untyped cmd))))
  ret <- runCommandMonad (undefined :: state) (runReal env cmd)
  liftIO (atomically (writeTQueue evs (Ok pid (toDyn ret))))
  case ret of
    Response _resp -> return Nothing
    Reference ref  -> return (Just ref)

linearisable :: forall state. StateModel state => Forest (Op state) -> Bool
linearisable = any' (go initialState)
  where
    go :: state -> Tree (Op state) -> Bool
    go s (Node (Op cmd ret) ts) =
      case runFake cmd s of
        Left err -> undefined
        Right (s', resp') ->
          case ret of
            Response resp ->
              resp == resp' && any' (go s') ts
            Reference _ref ->
              any' (go s') ts

    any' :: (a -> Bool) -> [a] -> Bool
    any' _p [] = True
    any'  p xs = any p xs

runParallelCommands :: forall state.
  (StateModel state, MonadIO (CommandMonad state),
  MonadCatch (CommandMonad state), Typeable state, Typeable (Reference state),
  (forall ref resp. Show ref => Show (Command state ref resp)),
  Show (Reference state))
  => ParallelCommands state -> PropertyM (CommandMonad state) ()
runParallelCommands (ParallelCommands cmdss0) = do
  -- replicateM_ 10 $ do
  --  monitor (tabulate "Commands" (map constructorString (concat cmdss)))
  monitor (tabulate "Number of concurrent commands" (map (show . length) cmdss0))
  evs <- liftIO newTQueueIO :: PropertyM (CommandMonad state) (TQueue (Event state))
  go evs [] cmdss0
  hist <- History <$> liftIO (atomically (flushTQueue evs))
  -- counterexample (prettyHistory hist)
  assert (linearisable (interleavings hist))
  where
    go _evs _vars [] = return ()
    go evs vars (cmds : cmdss) = do
      mrefs <- liftIO $ mapConcurrently (\(Untyped cmd_) -> runParallelReal evs (sub vars) cmd_) cmds
      let vars' = vars ++ zip [length vars..] (map toDyn (catMaybes mrefs))
      go evs vars' cmdss


  {-
prop_concurrent :: Property
prop_concurrent = mapSize (min 20) $
  forAllConcProgram $ \(ConcProgram cmdss) -> monadicIO $ do
    monitor (classifyCommandsLength (concat cmdss))
    -- Rerun a couple of times, to avoid being lucky with the interleavings.
    monitor (tabulate "Commands" (map constructorString (concat cmdss)))
    monitor (tabulate "Number of concurrent commands" (map (show . length) cmdss))
    replicateM_ 10 $ do
      counter <- run newCounter
      queue <- run newTQueueIO
      run (mapM_ (mapConcurrently (concExec queue counter)) cmdss)
      hist <- History <$> run (atomically (flushTQueue queue))
      assertWithFail (linearisable step initModel (interleavings hist)) (prettyHistory hist)
  where
    constructorString :: Command -> String
    constructorString Incr {} = "Incr"
    constructorString Get  {} = "Get"

assertWithFail :: Monad m => Bool -> String -> PropertyM m ()
assertWithFail condition msg = do
  unless condition $
    monitor (counterexample ("Failed: " ++ msg))
  assert condition
-}
