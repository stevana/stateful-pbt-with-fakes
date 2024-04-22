{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Stateful where

import Control.Monad
import Control.Monad.Catch
import Data.Dynamic
import Data.Foldable
import Data.Kind
import Data.Void
import Control.Monad.IO.Class
import Test.QuickCheck hiding (Failure, Success)
import Test.QuickCheck.Monadic

------------------------------------------------------------------------

type Symbolic state = Command state (Var (Reference state))
type Concrete state = Command state (Reference state)

class ( Monad (CommandMonad state)
      , MonadIO (CommandMonad state)
      , MonadCatch (CommandMonad state)
      , Functor (Command state)
      , Functor (Response state)
      , Foldable (Response state)
      , Eq (Response state (Reference state))
      , Show (Command state (Var (Reference state)))
      , Show (Response state (Reference state))
      , Show (Response state (Var (Reference state)))
      , Show (Reference state)
      , Typeable (Reference state)
      , Typeable state
      ) => StateModel state where

  data Command  state :: Type -> Type
  data Response state :: Type -> Type

  type Reference state :: Type

  type Failure state :: Type
  type Failure state = Void

  type CommandMonad state :: Type -> Type
  type CommandMonad state = IO

  generateCommand :: state -> Gen (Symbolic state)

  shrinkCommand :: state -> Symbolic state -> [Symbolic state]
  shrinkCommand _state _cmd = []

  initialState :: state

  runFake :: Symbolic state -> state
          -> Either (Failure state) (state, Response state (Var (Reference state)))

  runReal :: Concrete state -> CommandMonad state (Response state (Reference state))

  monitoring :: (state, state) -> Concrete state -> Response state (Reference state)
             -> Property -> Property
  monitoring _states _cmd _resp = id

  commandName :: (Show (Command state ref), Show ref)
              => Command state ref -> String
  commandName = head . words . show

  runCommandMonad :: proxy state -> CommandMonad state a -> IO a

------------------------------------------------------------------------

-- XXX: Can be removed when StateModel.hs is removed?
type Env state = Var (Reference state) -> Reference state

data Var a = Var Int
  deriving (Show, Eq, Ord)

newtype NonFoldable a = NonFoldable a
  deriving (Eq, Show, Functor)

instance Foldable NonFoldable where
  foldMap _f (NonFoldable _x) = mempty

------------------------------------------------------------------------

-- * Generating and shrinking

newtype Commands state = Commands [Symbolic state]
deriving instance Show (Symbolic state) => Show (Commands state)

precondition :: StateModel state => state -> Symbolic state -> Bool
precondition s cmd = case runFake cmd s of
  Left _  -> False
  Right _ -> True

nextState :: StateModel state => state -> Symbolic state -> state
nextState s cmd = case runFake cmd s of
  Right (s', _) -> s'
  Left _err -> error "nextState: impossible, we checked for success in precondition"

instance StateModel state => Arbitrary (Commands state) where

  arbitrary :: Gen (Commands state)
  arbitrary = Commands <$> genCommands initialState
    where
      genCommands :: StateModel state
                  => state -> Gen [Symbolic state]
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
  shrink (Commands cmds) =
    map (Commands . prune . map fst) (shrinkList shrinker (withStates cmds))
    where
      shrinker (cmd, s) = [ (cmd', s) | cmd' <- shrinkCommand s cmd ]

withStates :: StateModel state => [Symbolic state] -> [(Symbolic state, state)]
withStates = go initialState
  where
    go _s []           = []
    go  s (cmd : cmds) = (cmd, s) : go (nextState s cmd) cmds

prune :: StateModel state => [Symbolic state] -> [Symbolic state]
prune = go initialState
  where
    go _s [] = []
    go  s (cmd : cmds)
      | precondition s cmd = cmd : go (nextState s cmd) cmds
      | otherwise          = go s cmds

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
runCommands (Commands cmds0) = go initialState [] cmds0
  where
    go :: state -> [(Int, Dynamic)] -> [Symbolic state]
       -> PropertyM (CommandMonad state) ()
    go _state _vars [] = return ()
    go  state  vars (cmd : cmds) = do
      case runFake cmd state of
        Left _err -> pre False
        Right (state', resp) -> do
          let name = commandName cmd
          monitor (tabulate "Commands" [name] . classify True name)
          let ccmd = fmap (sub vars) cmd
          cresp <- run (runReal ccmd)
          monitor (counterexample (show cmd ++ " --> " ++ show cresp))
          monitor (monitoring (state, state') ccmd cresp)
          let refs   = toList cresp
              vars'  = vars ++ zip [length vars..] (map toDyn refs)
              cresp' = fmap (sub vars') resp
              ok     = cresp == cresp'
          unless ok $
            monitor (counterexample ("Expected: " ++ show cresp' ++ "\nGot: " ++ show cresp))
          assert ok
          go state' vars' cmds

sub :: Typeable a => [(Int, Dynamic)] -> Var a -> a
sub vars (Var x) =
  case lookup x vars of
    Nothing -> error $ "discard: vars = " ++ show vars ++ ", var = " ++ show x -- discard
      -- ^ this can happen if a shrink step makes a variable unbound
    Just var_ ->
      case fromDynamic var_ of
        Nothing  -> error $ "variable " ++ show x ++ " has wrong type"
        Just var -> var
