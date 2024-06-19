{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Example.FileSystem.Test where

import Control.Monad
import qualified Data.Map as Map
import System.Directory
import System.IO
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Example.FileSystem.Fake
import Example.FileSystem.Real
import Stateful

------------------------------------------------------------------------

instance StateModel FakeFS where

  initialState :: FakeFS
  initialState = emptyFakeFS

  data Command FakeFS h
    = MkDir Dir
    | Open  File
    | Write h String
    | Close h
    | Read  File
    deriving (Eq, Show, Functor, Foldable)

  data Response FakeFS h
    = MkDir_ ()
    | Open_ h
    | Write_ ()
    | Close_ ()
    | Read_ String
    deriving (Eq, Show, Functor, Foldable)

  type Reference FakeFS = Handle

  type PreconditionFailure FakeFS = PrecondFail

  type CommandMonad FakeFS = IO

  generateCommand :: FakeFS -> Gen (Command FakeFS FHandle)
  generateCommand s = oneof $ concat
    [ withoutHandle
    , if null (Map.keys (open s)) then [] else withHandle
    ]
    where
      withoutHandle :: [Gen (Command FakeFS FHandle)]
      withoutHandle =
        [ MkDir <$> genDir
        , Open <$> genFile
        -- XXX: introduce bug in generation which causes OpenTwo never to happen.
        -- XXX: can further be improved using coverTable
        -- ] ++ [ Open <$> genFile | null (Map.keys (open s)) ] ++
        , Read <$> if null (Map.keys (files s))
                   then genFile
                   else genWrittenFile
        ]

      withHandle :: [Gen (Command FakeFS FHandle)]
      withHandle =
        [ Write <$> genHandle <*> genString
        , Close <$> genHandle
        ]

      genHandle :: Gen FHandle
      genHandle = Var <$> choose (0, Map.size (open s) - 1)

      genWrittenFile :: Gen File
      genWrittenFile = elements (Map.keys (files s))

      genDir :: Gen Dir
      genDir = do
        n <- choose (0, 3)
        Dir <$> replicateM n (elements ["x", "y", "z"])

      genFile :: Gen File
      genFile = File <$> genDir <*> elements ["a", "b", "c"]

      genString :: Gen String
      genString = sized $ \n -> replicateM n (elements "ABC")

  runFake (MkDir d)   = assoc MkDir_ . fMkDir d
  runFake (Open f)    = assoc Open_  . fOpen f
  runFake (Write h s) = assoc Write_ . fWrite h s
  runFake (Close h)   = assoc Close_ . fClose h
  runFake (Read f)    = assoc Read_  . fRead f

  runReal :: Command FakeFS Handle -> IO (Response FakeFS Handle)
  runReal (MkDir d)   = MkDir_ <$> rMkDir d
  runReal (Open f)    = Open_  <$> rOpen f
  runReal (Write h s) = Write_ <$> rWrite h s
  runReal (Close h)   = Close_ <$> rClose h
  runReal (Read f)    = Read_  <$> rRead f

  monitoring :: (FakeFS, FakeFS) -> Command FakeFS Handle -> Response FakeFS Handle
             -> Property -> Property
  monitoring (_s, _s') Read {} (Read_ s) = classify (not (null s)) (show SuccessfulRead)
  monitoring (_s, s') _cmd _resp =
    classify (length (Map.keys (open s')) >= 2) (show OpenTwo)

------------------------------------------------------------------------

data Tag = OpenTwo | SuccessfulRead
  deriving Show

assoc :: (a -> Response FakeFS FHandle) -> (Either e a, FakeFS)
      -> Either e (FakeFS, Response FakeFS FHandle)
assoc _f (Left e, _s) = Left e
assoc f  (Right x, s) = Right (s, f x)


prop_fileSystem :: Commands FakeFS -> Property
prop_fileSystem cmds = monadicIO $ do
  runCommands cmds
  run cleanup
  assert True
  where
    cleanup :: IO ()
    cleanup = do
      removePathForcibly root
      createDirectory root

showLabelledExamples :: IO ()
showLabelledExamples = labelledExamples prop_fileSystem
