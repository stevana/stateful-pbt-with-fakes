{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}

module Example.KeyValueStore.Test where

import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Coerce
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Example.KeyValueStore.Real
import Parallel
import Stateful

------------------------------------------------------------------------

data State = State
  { initialised :: Bool
  , store :: Map Key Int
  }
  deriving (Eq, Ord, Show)

instance StateModel State where

  initialState = State False Map.empty

  type Reference State = Store Int

  data Command State ref
    = New
    | Write  ref Key Int
    | Read   ref Key
    | Delete ref Key
    deriving (Show, Functor, Foldable)

  data Response State ref
    = New_ ref
    | Write_ ()
    | Read_ (Maybe Int)
    | Delete_ ()
    deriving (Eq, Show, Functor, Foldable)

  generateCommand s
    | not (initialised s) = return New
    | otherwise           = frequency
        [ (5, Write  <$> pure (Var 0) <*> arbitraryKey <*> arbitrary)
        , (3, Read   <$> pure (Var 0) <*> arbitraryKey)
        , (2, Delete <$> pure (Var 0) <*> arbitraryKey)
        ]
    where
      arbitraryKey = frequency $
        [ (4, elements (Map.keys (store s))) | not (Map.null (store s))] ++
        [ (1, elements ["a", "b", "c", "d", "e", "f"]) ]

  type PreconditionFailure State = ()

  runFake New s
    | not (initialised s) = return (s { initialised = True }, New_ (Var 0))
    | otherwise           = Left ()
  runFake (Write _ref key value) s = return (s { store = Map.insert key value (store s) }, Write_ ())
  runFake (Read _ref key)        s = return (s, Read_ (Map.lookup key (store s)))
  runFake (Delete _ref key)      s = return (s { store = Map.delete key (store s)}, Delete_ ())

  runReal New                   = New_    <$> newStore
  runReal (Write ref key value) = Write_  <$> writeKV ref key value
  runReal (Read  ref key)       = Read_   <$> readKV ref key
  runReal (Delete ref key)      = Delete_ <$> deleteKV ref key

instance ParallelModel State where
  runCommandMonad _ = id

prop_keyValueStore :: Commands State -> Property
prop_keyValueStore cmds = monadicIO $ do
  runCommands cmds
  assert True

prop_parallelKeyValueStore :: ParallelCommands State -> Property
prop_parallelKeyValueStore cmds = monadicIO $ do
  monitor (tabulate "Disjoint concurrent writes" (map show (concWrites cmds)))
  replicateM_ 10 (runParallelCommands cmds)
  assert True

concWrites :: ParallelCommands State -> [Int]
concWrites (ParallelCommands forks) = map disjointWrites' (coerce forks)

  -- XXX: clean up
disjointWrites' :: [Command State (Var (Reference State))] -> Int
disjointWrites' = go . filter isWrite
  where
    isWrite Write {} = True
    isWrite _ = False

    writeKey   (Write _ key _value) = key
    writeValue (Write _ _key value) = value

    go [] = 0
    go [_] = 0
    -- go writes = map writeKey writes
    go [Write _ key value, Write _ key' value'] = if key /= key' && value /= value' then 2 else 0
    go [Write _ key value, Write _ key' value', Write _ key'' value''] =
      if key /= key' && key /= key'' && key' /= key'' && value /= value' && value /= value'' && value' /= value'' then 3 else 0
    go _ = error "impossible"
