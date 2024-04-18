{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module FileSystem.Test where

import System.IO
import System.Directory
import Control.Exception (fromException)
import Control.Monad
import qualified Data.Map as Map
import Test.QuickCheck
import Test.QuickCheck.Monadic

import FileSystem.Interface
import FileSystem.Fake
import StateModel

------------------------------------------------------------------------

newtype State = State { unState :: Mock }

instance StateModel State where

  data Command State a where
    MkDir :: Dir -> Command State ()
    Open  :: File -> Command State (Var Handle)
    Write :: Var Handle -> String -> Command State ()
    Close :: Var Handle -> Command State ()
    Read  :: File -> Command State String

  type Reference State = Handle

  type Failure State = Err

  type CommandMonad State = IO

  generateCommand :: State -> Gen (Untyped (Command State))
  generateCommand (State s) = oneof $ concat
    [ withoutHandle
    , if null (Map.keys (open s)) then [] else withHandle
    ]
    where
      withoutHandle :: [Gen (Untyped (Command State))]
      withoutHandle =
        [ Untyped <$> (MkDir <$> genDir)
        , Untyped <$> (Open <$> genFile)
        , Untyped <$> (Read <$> if null (Map.keys (files s))
                                then genFile
                                else genWrittenFile s)
        ]

      withHandle :: [Gen (Untyped (Command State))]
      withHandle =
        [ Untyped <$> (Write <$> genHandle s <*> genString)
        , Untyped <$> (Close <$> genHandle s)
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

  runFake (MkDir d)   = assoc . mMkDir d . unState
  runFake (Open f)    = assoc . mOpen f . unState
  runFake (Write h s) = assoc . mWrite h s . unState
  runFake (Close h)   = assoc . mClose h . unState
  runFake (Read f)    = assoc . mRead f . unState

  runReal :: Env State -> Command State a -> IO (Return State a)
  runReal _env (MkDir d)  = Response <$> iMkDir (real root) d
  runReal _env (Open f)   = Reference  <$> iOpen (real root) f
  runReal env (Write h s) = Response <$> iWrite (real root) (env h) s
  runReal env (Close h)   = Response <$> iClose (real root) (env h)
  runReal _env (Read f)   = Response <$> iRead (real root) f

  abstractFailure _state ex = fromIOError =<< fromException ex

  -- XXX: this isn't right
  monitoring (_state, State state') (Open _f) (Right _h)
    | Map.size (open state') >= 2 = tabulate "Tags" [show OpenTwo]
    | otherwise                   = id
  monitoring (State state, _state') (Read f) (Right _s)
    | f `elem` Map.keys (files state) = tabulate "Tags" [show SuccessfulRead]
    | otherwise = id
  monitoring _states _cmd _resp = id

deriving instance Show (Command State a)

------------------------------------------------------------------------

data Tag = OpenTwo | SuccessfulRead
  deriving Show

root :: FilePath
root = "/tmp/qc-test"

assoc :: (Either e a, Mock) -> Either e (State, a)
assoc (Left e, _s) = Left e
assoc (Right x, s) = Right (State s, x)


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
