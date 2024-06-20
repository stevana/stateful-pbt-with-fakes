{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Stateful where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Foldable
import Data.Kind
import Data.Coerce
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Void
import Test.QuickCheck hiding (Failure, Success)
import Test.QuickCheck.Monadic

------------------------------------------------------------------------

class ( Monad (CommandMonad state)
      , MonadIO (CommandMonad state)
      , MonadCatch (CommandMonad state)
      , Functor (Command state)
      , Foldable (Command state)
      , Functor (Response state)
      , Foldable (Response state)
      , Eq (Response state (Reference state))
      , Show state
      , Show (Command state (Var (Reference state)))
      , Show (Response state (Reference state))
      , Show (Response state (Var (Reference state)))
      , Show (Reference state)
      , Show (PreconditionFailure state)
-- start snippet StateModel
      ) => StateModel state where

  -- If we think of the system under test as a black box, then commands are the
  -- inputs and responses the outputs to the black box.
  data Command  state :: Type -> Type
  data Response state :: Type -> Type

  -- Sometimes a command needs to refer to a previous response, e.g. when a file
  -- is opened we get a handle which is later refered to when writing or reading
  -- form the file. File handles, and similar constructs, are called references
  -- and can be part of commands and responses.
  type Reference state :: Type
  type Reference state = Void

  -- Not all commands are valid in all states. Pre-conditions allow the user to
  -- specify when a command is safe to execute, for example we cannot write or
  -- read to or from an unopened file. The `PreconditionFailure` data type
  -- allows the user to create custom pre-condition failures. By default now
  -- pre-condition failures are allowed, thus the `Void` (empty) type.
  type PreconditionFailure state :: Type
  type PreconditionFailure state = Void


  generateCommand :: state -> Gen (Command state (Var (Reference state)))

  shrinkCommand :: state -> Command state (Var (Reference state))
                -> [Command state (Var (Reference state))]
  shrinkCommand _state _cmd = []

  initialState :: state

  runFake :: Command state (Var (Reference state)) -> state
          -> Either (PreconditionFailure state)
                    (state, Response state (Var (Reference state)))

  runReal :: Command state (Reference state)
          -> CommandMonad state (Response state (Reference state))

  monitoring :: (state, state)
             -> Command state (Reference state)
             -> Response state (Reference state)
             -> Property -> Property
  monitoring _states _cmd _resp = id

  commandName :: (Show (Command state ref), Show ref)
              => Command state ref -> String
  commandName = head . words . show

  -- Most often the result of executing a command against the system under test
  -- will live in the IO monad, but sometimes it can be useful to be able a
  -- choose another monad.
  type CommandMonad state :: Type -> Type
  type CommandMonad state = IO
-- end snippet StateModel

------------------------------------------------------------------------

-- start snippet Var
data Var a = Var Int
  deriving stock (Show, Eq, Ord)
-- end snippet Var

-- start snippet NonFoldable
newtype NonFoldable a = NonFoldable a
  deriving stock (Eq, Show)

instance Functor NonFoldable where
  fmap f (NonFoldable x) = NonFoldable (f x)

instance Foldable NonFoldable where
  foldMap _f (NonFoldable _x) = mempty
-- end snippet NonFoldable

------------------------------------------------------------------------

-- * Generating and shrinking

-- start snippet Commands
newtype Commands state = Commands
  { unCommands :: [Command state (Var (Reference state))] }
-- end snippet Commands
deriving stock instance Show (Command state (Var (Reference state))) => Show (Commands state)

-- The precondition for a command is the same as the fake returning a value.
-- start snippet precondition
precondition :: StateModel state
             => state -> Command state (Var (Reference state)) -> Bool
precondition s cmd = case runFake cmd s of
  Left _  -> False
  Right _ -> True
-- end snippet precondition

-- Get the next state by running the fake. Assumes that the precondition holds.
-- start snippet nextState
nextState :: StateModel state
          => state -> Command state (Var (Reference state)) -> state
nextState s cmd = case runFake cmd s of
  Right (s', _) -> s'
  Left _err -> error "nextState: impossible, we checked for success in precondition"
-- end snippet nextState

-- start snippet Arbitrary
instance StateModel state => Arbitrary (Commands state) where

  arbitrary :: Gen (Commands state)
  arbitrary = Commands <$> genCommands initialState
    where
      genCommands :: StateModel state
                  => state -> Gen [Command state (Var (Reference state))]
      genCommands s = sized $ \n ->
        let
          w = n `div` 2 + 1
        in
          frequency
            [ (1, return [])
            , (w, do mcmd <- generateCommand s `suchThatMaybe` precondition s
                     case mcmd of
                       Nothing  -> return []
                       Just cmd -> (cmd :) <$> genCommands (nextState s cmd))
            ]

  shrink :: Commands state -> [Commands state]
  shrink = pruneShrinks . possibleShrinks
    where
      possibleShrinks :: Commands state -> [Commands state]
      possibleShrinks = map (Commands . map fst) . shrinkList shrinker
                      . withStates initialState . unCommands
        where
          shrinker (cmd, s) = [ (cmd', s) | cmd' <- shrinkCommand s cmd ]

          withStates :: StateModel state
                     => state -> [Command state (Var (Reference state))]
                     -> [(Command state (Var (Reference state)), state)]
          withStates s0 = go s0 []
            where
              go _s acc []           = reverse acc
              go  s acc (cmd : cmds) = go (nextState s cmd) ((cmd, s) : acc) cmds

      pruneShrinks :: [Commands state] -> [Commands state]
      pruneShrinks = coerce . filter (not . null)
                   . map (go initialState Set.empty [] . unCommands)
        where
          go _s _vars acc [] = reverse acc
          go  s  vars acc (cmd : cmds)
            | not (scopeCheck vars cmd) = go s vars acc cmds
            | otherwise = case runFake cmd s of
                Left _preconditionFailure -> go s vars acc cmds
                Right (s', resp) ->
                  let
                    returnedVars = Set.fromList (toList resp)
                    vars' = returnedVars `Set.union` vars
                  in
                    go s' vars' (cmd : acc) cmds

scopeCheck :: Foldable (Command state)
           => Set (Var a) -> Command state (Var a) -> Bool
scopeCheck varsInScope cmd = usedVars `Set.isSubsetOf` varsInScope
  where
    usedVars = Set.fromList (toList cmd)
-- end snippet Arbitrary

------------------------------------------------------------------------

-- * Running

-- The response might contain new references, e.g. `Spawn` returns a `ThreadId`,
-- these need to be added to the environment, but a response can also contain
-- referenses that shouldn't be added to the environment, e.g. `WhereIs` also
-- returns a `ThreadId`.
--
-- The design I went with here is to add a `newtype Unfoldable a = Unfoldable a`
-- whose `Foldable` instance has a `toList` that returns the the empty list.
-- That way we can wrap the `ThreadId` of `WhereIs` in `Unfoldable` and always
-- add all references that the response returns to the environment.
--
-- Another option would be to introduce a new type class `ReturnsReferences` and
-- ask the user to manually implement it.

-- start snippet runCommands
runCommands :: forall state. StateModel state
            => Commands state -> PropertyM (CommandMonad state) ()
runCommands (Commands cmds0) = go initialState emptyEnv cmds0
  where
    go :: state -> Env state -> [Command state (Var (Reference state))]
       -> PropertyM (CommandMonad state) ()
    go _state _env [] = return ()
    go  state  env (cmd : cmds) = do
      case runFake cmd state of
        Left err -> do
          monitor (counterexample ("Preconditon failed: " ++ show err))
          assert False
        Right (state', resp) -> do
          let name = commandName cmd
          monitor (tabulate "Commands" [name] . classify True name)
          -- Here we substitute all symbolic references for real ones:
          let ccmd = fmap (lookupEnv env) cmd
          cresp <- run (runReal ccmd)
          monitor (counterexample (show cmd ++ " --> " ++ show cresp))
          monitor (monitoring (state, state') ccmd cresp)
          -- Here we collect all references from the response and store it in
          -- our environment, so that subsequence commands can be substituted.
          let refs   = toList cresp
              env'   = extendEnv env (zip [sizeEnv env..] refs)
              cresp' = fmap (lookupEnv env') resp
              ok     = cresp == cresp'
          unless ok $
            monitor (counterexample ("Expected: " ++ show cresp' ++ "\nGot: " ++ show cresp))
          -- And finally here's where we assert that the model and the real
          -- implementation agree.
          assert ok
          go state' env' cmds
-- end snippet runCommands

-- start snippet Env
newtype Env state = Env { unEnv :: IntMap (Reference state) }
-- end snippet Env

sizeEnv :: Env state -> Int
sizeEnv (Env im) = IntMap.size im

emptyEnv :: Env state
emptyEnv = Env IntMap.empty

lookupEnv :: Env state -> Var (Reference state) -> Reference state
lookupEnv (Env im) (Var i) = im IntMap.! i

extendEnv :: Env state -> [(Int, Reference state)] -> Env state
extendEnv (Env im) refs = Env (im `IntMap.union` IntMap.fromList refs)
