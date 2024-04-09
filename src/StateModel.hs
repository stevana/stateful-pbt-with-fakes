{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module StateModel where

import Control.Monad.Catch
import Data.Dynamic
import Data.Either
import Data.Kind
import Test.QuickCheck hiding (Failure, Success)
import Test.QuickCheck.Monadic

------------------------------------------------------------------------

class StateModel state where
  data Command state :: forall k. k -> Type -> Type
  type Reference state :: Type
  type Failure state :: Type

  type CommandMonad state :: Type -> Type
  type CommandMonad state = IO

  generateCommand :: state -> Gen (Untyped (Command state (Var (Reference state))))

  shrinkCommand :: state -> Untyped (Command state (Var (Reference state)))
                -> [Untyped (Command state (Var (Reference state)))]
  shrinkCommand _state _cmd = []

  initialState :: state

  runFake :: Command state (Var (Reference state)) resp -> state
          -> Either (Failure state) (state, resp)

  runReal :: Env state -> Command state (Var (Reference state)) resp
          -> CommandMonad state (Return state resp)

  abstractFailure :: state -> SomeException -> Maybe (Failure state)

  monitoring :: (state, state) -> Command state (Var (Reference state)) resp
             -> Either (Reference state) resp -> Property -> Property
  monitoring _states _cmd _resp = id

  commandName :: (forall ref. Show ref => Show (Command state ref resp),
                  Show resp)
              => Command state (Var (Reference state)) resp -> String
  commandName = head . words . show

  runCommandMonad :: state -> CommandMonad state a -> IO a

------------------------------------------------------------------------

type Untyped :: (Type -> Type) -> Type
data Untyped f where
  Untyped :: (Typeable a, Eq a, Show a) => f a -> Untyped f
deriving instance (forall a. Show (f a)) => Show (Untyped f)

type Env state = Var (Reference state) -> Reference state

data Var a = Var Int
  deriving (Show, Eq, Ord)

data Return state resp
  = Reference (Reference state)
  | Response resp
deriving instance (Show (Reference state), Show resp) => Show (Return state resp)

------------------------------------------------------------------------

-- * Generating and shrinking

newtype Commands state = Commands [Untyped (Command state (Var (Reference state)))]

deriving instance Show (Untyped (Command state (Var (Reference state)))) => Show (Commands state)

precondition :: StateModel state => state -> Untyped (Command state (Var (Reference state))) -> Bool
precondition s (Untyped cmd) = isRight (runFake cmd s)

nextState :: StateModel state => state -> Untyped (Command state (Var (Reference state))) -> state
nextState s (Untyped cmd) = case runFake cmd s of
  Right (s', _) -> s'
  Left _err -> error "nextState: impossible, we checked for success in precondition"

instance StateModel state => Arbitrary (Commands state) where
  arbitrary = Commands <$> genCommands initialState
    where
      genCommands :: StateModel state
                  => state -> Gen [Untyped (Command state (Var (Reference state)))]
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

  shrink (Commands cmds) =
    map (Commands . prune . map fst) (shrinkList shrinker (withStates cmds))
    where
      shrinker (cmd, s) = [ (cmd', s) | cmd' <- shrinkCommand s cmd ]

withStates :: StateModel state
           => [Untyped (Command state (Var (Reference state)))]
           -> [(Untyped (Command state (Var (Reference state))), state)]
withStates = go initialState
  where
    go _s []           = []
    go  s (cmd : cmds) = (cmd, s) : go (nextState s cmd) cmds

prune :: StateModel state
      => [Untyped (Command state (Var (Reference state)))]
      -> [Untyped (Command state (Var (Reference state)))]
prune = go initialState
  where
    go _s [] = []
    go  s (cmd : cmds)
      | precondition s cmd = cmd : go (nextState s cmd) cmds
      | otherwise          = go s cmds

------------------------------------------------------------------------

-- * Running

newtype History state = History [Event state]

data Event state
  = forall resp. Success (Command state (Var (Reference state)) resp) resp
  | Failure (Failure state)

runCommands :: forall state. (StateModel state, Monad (CommandMonad state),
               MonadCatch (CommandMonad state), Eq (Failure state),
               (forall ref resp. Show ref => Show (Command state ref resp)),
               Show (Reference state), Typeable (Reference state))
            => Commands state -> PropertyM (CommandMonad state) (History state)
runCommands (Commands cmds0) = History <$> go initialState 0 [] [] cmds0
  where
    go :: state -> Int -> [(Int, Dynamic)] -> [Event state]
       -> [Untyped (Command state (Var (Reference state)))]
       -> PropertyM (CommandMonad state) [Event state]
    go _state _i _vars events [] = return (reverse events)
    go  state  i  vars events (Untyped cmd : cmds) = do
      pre (precondition state (Untyped cmd))
      let name = commandName cmd
      monitor (tabulate "Commands" [name] . classify True name)
      -- XXX:
      -- eRet <- run (try (runReal2 (subst (sub vars) cmd)))
      eRet <- run (try (runReal (sub vars) cmd))
      monitor (counterexample (show cmd ++ " --> " ++ show eRet))
      case (eRet, runFake cmd state) of
        (Left err, Left err') -> do
          case abstractFailure state err of
            Nothing -> assert False >> return (reverse events)
            Just err'' -> do
              assert (err'' == err')
              go state i vars (Failure err' : events) cmds
        (Right (Reference var), Right (state', resp')) -> do
          monitor (monitoring (state, state') cmd (Left var))
          go state' (i + 1) ((i, toDyn var) : vars) (Success cmd resp' : events) cmds
        (Right (Response resp), Right (state', resp')) -> do
          monitor (monitoring (state, state') cmd (Right resp))
          assert (resp == resp')
          go state' i vars (Success cmd resp : events) cmds
        (Left _, Right _) -> assert False >> return (reverse events)
        (Right _, Left _) -> assert False >> return (reverse events)

sub :: Typeable a => [(Int, Dynamic)] -> Var a -> a
sub vars (Var x) =
  case lookup x vars of
    Nothing -> discard
      -- ^ this can happen if a shrink step makes a variable unbound
    Just var_ ->
      case fromDynamic var_ of
        Nothing  -> error $ "variable " ++ show x ++ " has wrong type"
        Just var -> var
