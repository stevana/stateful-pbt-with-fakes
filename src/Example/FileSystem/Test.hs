{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Example.FileSystem.Test where

import Control.Monad
import qualified Data.Map as Map
import System.Directory
import System.IO
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Example.FileSystem.Fake
import Example.FileSystem.Interface
import Stateful

------------------------------------------------------------------------

newtype State = State { unState :: Mock }
  deriving Show

instance StateModel State where

  data Command State h
    = MkDir Dir
    | Open  File
    | Write h String
    | Close h
    | Read  File
    deriving (Eq, Show, Functor)

  data Response State h
    = MkDir_ ()
    | Open_ h
    | Write_ ()
    | Close_ ()
    | Read_ String
    deriving (Eq, Show, Functor, Foldable)

  type Reference State = Handle

  type Failure State = Err

  type CommandMonad State = IO

  generateCommand :: State -> Gen (Command State (Var Handle))
  generateCommand (State s) = oneof $ concat
    [ withoutHandle
    , if null (Map.keys (open s)) then [] else withHandle
    ]
    where
      withoutHandle :: [Gen (Command State (Var Handle))]
      withoutHandle =
        [ MkDir <$> genDir
        , Open <$> genFile
        , Read <$> if null (Map.keys (files s))
                   then genFile
                   else genWrittenFile s
        ]

      withHandle :: [Gen (Command State (Var Handle))]
      withHandle =
        [ Write <$> genHandle s <*> genString
        , Close <$> genHandle s
        ]

      genHandle :: Mock -> Gen MHandle
      genHandle mock = Var <$> choose (0, Map.size (open mock) - 1)

      genWrittenFile :: Mock -> Gen File
      genWrittenFile mock = elements (Map.keys (files mock))

      genDir :: Gen Dir
      genDir = do
        n <- choose (0, 3)
        Dir <$> replicateM n (elements ["x", "y", "z"])

      genFile :: Gen File
      genFile = File <$> genDir <*> elements ["a", "b", "c"]

      genString :: Gen String
      genString = sized $ \n -> replicateM n (elements "ABC")

  initialState :: State
  initialState = State emptyMock

  runFake (MkDir d)   = assoc MkDir_ . mMkDir d . unState
  runFake (Open f)    = assoc Open_  . mOpen f . unState
  runFake (Write h s) = assoc Write_ . mWrite h s . unState
  runFake (Close h)   = assoc Close_ . mClose h . unState
  runFake (Read f)    = assoc Read_  . mRead f . unState

  runReal :: Command State Handle -> IO (Response State Handle)
  runReal (MkDir d)   = MkDir_ <$> iMkDir (real root) d
  runReal (Open f)    = Open_  <$> iOpen (real root) f
  runReal (Write h s) = Write_ <$> iWrite (real root) h s
  runReal (Close h)   = Close_ <$> iClose (real root) h
  runReal (Read f)    = Read_  <$> iRead (real root) f

  runCommandMonad _ = id

------------------------------------------------------------------------

data Tag = OpenTwo | SuccessfulRead
  deriving Show

root :: FilePath
root = "/tmp/qc-test"

assoc :: (a -> Response State (Var Handle)) -> (Either e a, Mock) -> Either e (State, Response State (Var Handle))
assoc _f (Left e, _s) = Left e
assoc f  (Right x, s) = Right (State s, f x)


prop_fileSystem :: Commands State -> Property
prop_fileSystem cmds = monadicIO $ do
  run cleanup
  _ <- runCommands cmds
  assert True
  where
    cleanup :: IO ()
    cleanup = do
      removePathForcibly root
      createDirectory root
