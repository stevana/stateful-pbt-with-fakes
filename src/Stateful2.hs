{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Stateful2 where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.State
import Data.Foldable
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import GHC.Exts (IsList)
import Data.Kind
import Data.Void
import Test.QuickCheck hiding (Failure, Success)
import Test.QuickCheck.Monadic

------------------------------------------------------------------------

newtype Fake state a = Fake
  { unFake :: StateT state (StateT GenSym (Either (PreconditionFailure state))) a }
  deriving (Functor, Applicative, Monad, MonadState state)

newtype GenSym = GenSym Int

initialGenSym :: GenSym
initialGenSym = GenSym 0

genSym :: Fake state (Var (Reference state))
genSym = do
  GenSym i <- Fake (lift get)
  Fake (lift (put (GenSym (i + 1))))
  return (Var i)

evalRunFake :: Fake state a -> state -> GenSym -> Either (PreconditionFailure state) (a, state)
evalRunFake (Fake m) s gsym = evalStateT (runStateT m s) gsym

runRunFake :: Fake state a -> state -> GenSym -> Either (PreconditionFailure state) ((a, state), GenSym)
runRunFake (Fake m) s gsym = runStateT (runStateT m s) gsym

preFail :: PreconditionFailure state -> Fake state a
preFail e = Fake (lift (lift (Left e)))


class ( Monad (CommandMonad state)
      , MonadIO (CommandMonad state)
      , MonadCatch (CommandMonad state)
      , Functor (Command state)
      , Traversable (Command state)
      , Functor (Response state)
      , Foldable (Response state)
      , Eq (Response state (Reference state))
      , Show state
      , Show (Command state (Var (Reference state)))
      , Show (Response state (Reference state))
      , Show (Response state (Var (Reference state)))
      , Show (Reference state)
      , Show (PreconditionFailure state)
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

  runFake :: Command state (Var (Reference state))
          -> state -> Fake state (Response state (Var (Reference state)))

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

  -- If another command monad is used we need to provide a way run it inside the
  -- IO monad. This is only needed for parallel testing, because IO is the only
  -- monad we can execute on different threads.
  runCommandMonad :: proxy state -> CommandMonad state a -> IO a

------------------------------------------------------------------------

data Var a = Var Int
  deriving stock (Show, Eq, Ord)

newtype NonFoldable a = NonFoldable a
  deriving stock (Eq, Show)

instance Functor NonFoldable where
  fmap f (NonFoldable x) = NonFoldable (f x)

instance Foldable NonFoldable where
  foldMap _f (NonFoldable _x) = mempty

------------------------------------------------------------------------

-- * Generating and shrinking

newtype Commands state = Commands
  { unCommands :: [Command state (Var (Reference state))] }
  deriving newtype (Semigroup, Monoid, IsList)
deriving stock instance Show (Command state (Var (Reference state))) => Show (Commands state)

-- The precondition for a command is the same as the fake returning a value.
precondition :: StateModel state
             => state -> GenSym -> Command state (Var (Reference state)) -> Bool
precondition s gsym cmd = case evalRunFake (runFake cmd s) s gsym of
  Left _  -> False
  Right _ -> True

-- Get the next state by running the fake. Assumes that the precondition holds.
nextState :: StateModel state
          => state -> GenSym -> Command state (Var (Reference state)) -> (state, GenSym)
nextState s gsym cmd = case runRunFake (runFake cmd s) s gsym of
  Right ((_resp, s'), gsym') -> (s', gsym')
  Left _err -> error "nextState: impossible, we checked for success in precondition"

instance StateModel state => Arbitrary (Commands state) where

  arbitrary :: Gen (Commands state)
  arbitrary = Commands <$> genCommands initialState initialGenSym
    where
      genCommands :: StateModel state
                  => state -> GenSym -> Gen [Command state (Var (Reference state))]
      genCommands s gsym = sized $ \n ->
        let
          w = n `div` 2 + 1
        in
          frequency
            [ (1, return [])
            , (w, do mcmd <- generateCommand s `suchThatMaybe` precondition s gsym
                     case mcmd of
                       Nothing  -> return []
                       Just cmd -> (cmd :) <$> uncurry genCommands (nextState s gsym cmd))
            ]

  shrink :: Commands state -> [Commands state]
  shrink (Commands cmds) =
    map (Commands . prune . map fst)
        (shrinkList shrinker (snd (withStates initialState initialGenSym cmds)))
    where
      shrinker (cmd, s) = [ (cmd', s) | cmd' <- shrinkCommand s cmd ]

withStates :: StateModel state
           => state -> GenSym -> [Command state (Var (Reference state))]
           -> (state, [(Command state (Var (Reference state)), state)])
withStates s0 gsym0 = go s0 gsym0 []
  where
    go s _gsym acc []           = (s, reverse acc)
    go s  gsym acc (cmd : cmds) = uncurry go (nextState s gsym cmd) ((cmd, s) : acc) cmds

prune :: StateModel state
      => [Command state (Var (Reference state))] -> [Command state (Var (Reference state))]
prune = go initialState initialGenSym
  where
    go _s _gsym [] = []
    go  s  gsym (cmd : cmds)
      | precondition s gsym cmd = cmd : uncurry go (nextState s gsym cmd) cmds
      | otherwise               = go s gsym cmds

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
runCommands :: forall state. StateModel state
            => Commands state -> PropertyM (CommandMonad state) ()
runCommands (Commands cmds0) = go initialState initialGenSym emptyEnv cmds0
  where
    go :: state -> GenSym -> Env state -> [Command state (Var (Reference state))]
       -> PropertyM (CommandMonad state) ()
    go _s _gsym _env []           = return ()
    go  s  gsym  env (cmd : cmds) = do
      case runRunFake (runFake cmd s) s gsym of
        Left err -> do
          monitor (counterexample ("Preconditon failed: " ++ show err))
          assert False
        Right ((resp, s'), gsym') -> do
          let name = commandName cmd
          monitor (tabulate "Commands" [name] . classify True name)
          let !ccmd = fmap (lookupEnv env) cmd
          cresp <- run (runReal ccmd)
          monitor (counterexample (show cmd ++ " --> " ++ show cresp))
          monitor (monitoring (s, s') ccmd cresp)
          let refs   = toList cresp
              env'   = extendEnv_ env refs
              !cresp' = fmap (lookupEnv env') resp
              ok     = cresp == cresp'
          unless ok $
            monitor (counterexample ("Expected: " ++ show cresp' ++ "\nGot: " ++ show cresp))
          assert ok
          go s' gsym' env' cmds

newtype Env state = Env (IntMap (Reference state))
  deriving newtype (Semigroup, Monoid)
deriving stock instance Show (Reference state) => Show (Env state)

emptyEnv :: Env state
emptyEnv = mempty

lookupEnv :: Env state -> Var (Reference state) -> Reference state
lookupEnv (Env env) (Var x) =
  case IntMap.lookup x env of
    Nothing  -> discard -- ^ This can happen if a shrink step makes a variable unbound.
    Just ref -> ref

subst :: Traversable (Command state) => Env state -> Command state (Var (Reference state)) -> Maybe (Command state (Reference state))
subst (Env env) cmd = traverse (\(Var i) -> IntMap.lookup i env) cmd

extendEnv_ :: Env state -> [Reference state] -> Env state
extendEnv_ env@(Env im) refs = env <> Env (IntMap.fromList (zip [IntMap.size im ..] refs))
