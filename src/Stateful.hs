{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Stateful where

import Control.Monad
import Control.Monad.Catch
import Data.Dynamic
import Data.Either
import Data.Foldable
import Data.Kind
import Data.Void
import Test.QuickCheck hiding (Failure, Success)
import Test.QuickCheck.Monadic

------------------------------------------------------------------------

type Symbolic state = Command state (Var (Reference state))
type Concrete state = Command state (Reference state)

class ( Monad (CommandMonad state)
      , MonadCatch (CommandMonad state)
      , Functor (Command state)
      , Functor (Response state)
      , Foldable (Response state)
      , Eq (Response state (Reference state))
      , Show (Response state (Reference state))
      , Eq (Failure state)
      , Show (Command state (Var (Reference state)))
      , Show (Reference state)
      , Show (Failure state)
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
          -> Return (Failure state) (state, Response state (Var (Reference state)))

  runReal :: Concrete state -> CommandMonad state (Response state (Reference state))

  monitoring :: (state, state) -> Concrete state -> Response state (Reference state)
             -> Property -> Property
  monitoring _states _cmd _resp = id

  commandName :: (Show (Command state ref), Show ref)
              => Command state ref -> String
  commandName = head . words . show

  abstractFailure :: state -> SomeException -> Maybe (Failure state)
  abstractFailure _s _err = Nothing

  runCommandMonad :: state -> CommandMonad state a -> IO a

------------------------------------------------------------------------

-- By distinguishing between precondition failures and throws we can generate
-- negative tests (that are supposed to throw).
data Return e a
  = Precondition e
  | Throw e
  | Ok a
  deriving Show

instance Functor (Return e) where
  fmap _f (Precondition e) = Precondition e
  fmap _f (Throw e)        = Throw e
  fmap  f (Ok x)           = Ok (f x)

instance Applicative (Return e) where
  pure = Ok
  mf <*> mx = undefined

instance Monad (Return e) where
  Precondition e >>= _k = Precondition e
  Throw e        >>= _k = Throw e
  Ok x           >>=  k = k x

type Env state = Var (Reference state) -> Reference state

data Var a = Var Int
  deriving (Show, Eq, Ord)

------------------------------------------------------------------------

-- * Generating and shrinking

newtype Commands state = Commands [Symbolic state]
deriving instance Show (Symbolic state) => Show (Commands state)

precondition :: StateModel state => state -> Symbolic state -> Bool
precondition s cmd = case runFake cmd s of
  Precondition _ -> False
  Ok _ -> True
  Throw _ -> True

nextState :: StateModel state => state -> Symbolic state -> state
nextState s cmd = case runFake cmd s of
  Ok (s', _) -> s'
  Precondition _err -> error "nextState: impossible, we checked for success in precondition"
  Throw _err -> s

instance StateModel state => Arbitrary (Commands state) where
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

  shrink (Commands cmds) =
    map (Commands . prune . map fst) (shrinkList shrinker (withStates cmds))
    where
      shrinker (cmd, s) = [ (cmd', s) | cmd' <- shrinkCommand s cmd ]

withStates :: StateModel state
           => [Symbolic state]
           -> [(Symbolic state, state)]
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

newtype History state = History [Event state]

data Event state
  = Success (Concrete state) (Response state (Reference state))
  | Failure (Concrete state) (Failure state)

runCommands :: forall state. StateModel state
            => Commands state -> PropertyM (CommandMonad state) (History state)
runCommands (Commands cmds0) = History <$> go initialState 0 [] [] cmds0
  where
    go :: state -> Int -> [(Int, Dynamic)] -> [Event state]
       -> [Symbolic state]
       -> PropertyM (CommandMonad state) [Event state]
    go _state _i _vars events [] = return (reverse events)
    go  state  i  vars events (cmd : cmds) = do
      pre (precondition state cmd)
      let name = commandName cmd
          cmd' = fmap (sub vars) cmd
      monitor (tabulate "Commands" [name] . classify True name)
      eResp <- run (try (runReal cmd'))
      monitor (counterexample (show cmd ++ " --> " ++ show eResp))
      let eResp' = runFake cmd state
      case (eResp, eResp') of
        (Left err, Throw err') -> do
          monitor (counterexample "err")
          assert (abstractFailure state err == Just err')
          go state i vars (Failure cmd' err' : events) cmds
        (Right resp, Ok (state', resp')) -> do
          monitor (monitoring (state, state') cmd' resp)
          -- The response might contain new references, e.g. `Spawn` returns a
          -- `ThreadId`, these need to be added to the environment, but a
          -- response can also contain referenses that shouldn't be added to the
          -- environment, e.g. `WhereIs` also returns a `ThreadId`. The design
          -- choice I went for here is to check if the returned references are
          -- disjoint from the environment, if so we assume it's a spawn-like
          -- operation, while if they intersect we assume a whereis-like
          -- operation.

          -- Another option might be to make a `newtype Unfoldable a =
          -- Unfoldable a` whose `Foldable` instance has a `toList` that returns
          -- the the empty list. That way we can wrap the `ThreadId` of
          -- `WhereIs` in `Unfoldable` and always add all references that the
          -- response returns to the environment.

          -- XXX: This can be improved...
          --   1. new refs always +1 of length vars (there can be more than one new ref)
          --   2. ops that return a new ref and old refs can't be handled here
          let refs | toList resp' `disjoint` map (Var . fst) vars = toList resp
                   | otherwise = []
              vars' | null refs = vars
                    | otherwise = vars ++ zip [i..] (map toDyn refs)
          when (resp /= fmap (sub vars') resp') $
            monitor (counterexample ("\nExpected: " ++ show (fmap (sub vars') resp') ++ "\nGot: " ++ show resp))
          assert (resp == fmap (sub vars') resp')
          go state' (i + length refs) vars' (Success cmd' resp : events) cmds
        (Left _, Ok (_state', _resp)) -> do
          monitor (counterexample "Bla")
          assert False
          return (reverse events)
        (Right resp, Throw err) -> do
          let state' = nextState state cmd
          -- XXX: Why doesn't this that bigJug = 4 in the DieHard example?
          monitor (counterexample ("Got: " ++ show resp ++ "\nExpected: " ++ show err))
          monitor (monitoring (state, state') cmd' resp)
          assert False
          return (reverse events)

disjoint :: Eq a => [a] -> [a] -> Bool
disjoint xs ys = all (`notElem` ys) xs

sub :: Typeable a => [(Int, Dynamic)] -> Var a -> a
sub vars (Var x) =
  case lookup x vars of
    Nothing -> discard
      -- ^ this can happen if a shrink step makes a variable unbound
    Just var_ ->
      case fromDynamic var_ of
        Nothing  -> error $ "variable " ++ show x ++ " has wrong type"
        Just var -> var
