-- Stolen from https://www.well-typed.com/blog/2019/01/qsm-in-depth/

module Example.FileSystem.Fake (
    -- * Paths
    Dir(..)
  , File(..)
  , dirFP
  , fileFP
    -- * Errors
  , Err(..)
  , fromIOError
    -- * Mock file system
  , MHandle
  , Mock(..)
  , MockOp
  , emptyMock
  , mMkDir
  , mOpen
  , mWrite
  , mClose
  , mRead
  ) where

import           Control.Exception
import           Data.Map         (Map)
import           Data.Set         (Set)
import qualified GHC.IO.Exception as GHC
import           System.FilePath  ((</>))
import           System.IO (Handle)
import           System.IO.Error

import qualified Data.List as List
import qualified Data.Map  as Map
import qualified Data.Set  as Set

import Stateful (Var(Var))

{-------------------------------------------------------------------------------
  Paths
-------------------------------------------------------------------------------}

data Dir = Dir [String]
  deriving (Show, Eq, Ord)

parent :: Dir -> Dir
parent (Dir fp) = Dir (init fp)

data File = File {fileDir :: Dir, fileName :: String}
  deriving (Show, Eq, Ord)

dirFP :: FilePath -> Dir -> FilePath
dirFP root (Dir d) = List.foldl' (</>) root d

fileFP :: FilePath -> File -> FilePath
fileFP root (File d f) = dirFP root d </> f

{-------------------------------------------------------------------------------
  Errors
-------------------------------------------------------------------------------}

data Err =
    AlreadyExists
  | DoesNotExist
  | HandleClosed
  | Busy
  deriving (Show, Eq)

instance Exception Err

fromIOError :: IOError -> Maybe Err
fromIOError e =
    case ioeGetErrorType e of
      GHC.AlreadyExists    -> Just AlreadyExists
      GHC.NoSuchThing      -> Just DoesNotExist
      GHC.ResourceBusy     -> Just Busy
      GHC.IllegalOperation -> Just HandleClosed
      _otherwise           -> Nothing

{-------------------------------------------------------------------------------
  Mock implementation
-------------------------------------------------------------------------------}

type MHandle = Var Handle

data Mock = M {
    dirs  :: Set Dir
  , files :: Map File String
  , open  :: Map MHandle File
  , next  :: MHandle
  }
  deriving Show

emptyMock :: Mock
emptyMock = M (Set.singleton (Dir [])) Map.empty Map.empty (Var 0)

type MockOp a = Mock -> (Either Err a, Mock)

mMkDir :: Dir -> MockOp ()
mMkDir d m@(M ds fs hs n)
  | d        `Set.member`    ds = (Left AlreadyExists, m)
  | parent d `Set.notMember` ds = (Left DoesNotExist, m)
  | otherwise                   = (Right (), M (Set.insert d ds) fs hs n)

mOpen :: File -> MockOp MHandle
mOpen f m@(M ds fs hs n@(Var n_))
  | alreadyOpen   = (Left Busy, m)
  | not dirExists = (Left DoesNotExist, m)
  | fileExists    = (Right n, M ds fs hs' n')
  | otherwise     = (Right n, M ds (Map.insert f "" fs) hs' n')
  where
    hs' = Map.insert n f hs
    n'  = Var (succ n_)

    fileExists  =         f `Map.member` fs
    dirExists   = fileDir f `Set.member` ds
    alreadyOpen = f `List.elem` Map.elems hs

mWrite :: MHandle -> String -> MockOp ()
mWrite h s m@(M ds fs hs n)
  | Just f <- Map.lookup h hs = (Right (), M ds (Map.adjust (++ s) f fs) hs n)
  | otherwise                 = (Left HandleClosed, m)

mClose :: MHandle -> MockOp ()
mClose h (M ds fs hs n) = (Right (), M ds fs (Map.delete h hs) n)

mRead :: File -> MockOp String
mRead f m@(M _ fs hs _)
  | alreadyOpen               = (Left Busy         , m)
  | Just s <- Map.lookup f fs = (Right s           , m)
  | otherwise                 = (Left DoesNotExist , m)
  where
    alreadyOpen = f `List.elem` Map.elems hs
