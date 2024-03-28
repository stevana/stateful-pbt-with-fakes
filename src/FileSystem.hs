{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GADTs #-}

module FileSystem where

import Control.Monad
import qualified Data.Map as Map
import System.IO
import GHC.Generics
import Test.QuickCheck

  {-
import StateModel
import FileSystem.Fake
import References
import qualified Rank2

------------------------------------------------------------------------

type State = Mock

instance StateModel State where
  data Command State a ref where
    MkDir :: Dir -> Command State () ref
    Open  :: File -> Command State (Reference Handle ref) ref
    Write :: Reference Handle ref -> String -> Command State () ref
    Close :: Reference Handle ref -> Command State () ref
    Read  :: File -> Command State String ref

  data Failure State = Err

  type CommandMonad State = IO

  generateCommand = undefined

-- deriving instance Generic1 (Command State resp)
-- instance Rank2.Functor (Command State resp) where
-- instance Rank2.Foldable (Command State resp) where
-- instance Rank2.Traversable (Command State resp) where


 -- deriving instance Show h => Show (Command h a)

genHandle :: State -> Gen (Symbolic Handle)
genHandle mock = choose (0, Symbolic (Var (Map.size (open mock) - 1)))

genWrittenFile :: State -> Gen File
genWrittenFile mock = elements (Map.keys (files mock))

genCommand :: State -> Gen (Untyped (Command State) Symbolic)
genCommand s = oneof $ concat
  [ withoutHandle
  , if null (Map.keys (open s)) then [] else withHandle
  ]
  where
    withoutHandle :: [Gen (Untyped (Command State) Symbolic)]
    withoutHandle =
      [ Untyped <$> (MkDir <$> genDir)
      , Untyped <$> (Open <$> genFile)
      , Untyped <$> (Read <$> if null (Map.keys (files s)) then genFile else genWrittenFile s)
      ]

    withHandle :: [Gen (Untyped (Command State) Symbolic)]
    withHandle =
      [ Untyped <$> (Write <$> genHandle s <*> genString)
      , Untyped <$> (Close <$> genHandle s)
      ]

genDir :: Gen Dir
genDir = do
    n <- choose (0, 3)
    Dir <$> replicateM n (elements ["x", "y", "z"])

genFile :: Gen File
genFile = File <$> genDir <*> elements ["a", "b", "c"]

genString :: Gen String
genString = sized $ \n -> replicateM n (elements "ABC")

-}
