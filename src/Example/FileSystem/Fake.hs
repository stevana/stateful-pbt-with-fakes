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
    -- * FakeFS file system
  , FHandle
  , FakeFS(..)
  , FakeOp
  , emptyFakeFS
  , mMkDir
  , mOpen
  , mWrite
  , mClose
  , mRead
  ) where

import Control.Exception
import Data.Map (Map)
import Data.Set (Set)
import qualified GHC.IO.Exception as GHC
import System.FilePath ((</>))
import System.IO (Handle)
import System.IO.Error

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

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
  FakeFS implementation
-------------------------------------------------------------------------------}

type FHandle = Var Handle

data FakeFS = F {
    dirs  :: Set Dir
  , files :: Map File String
  , open  :: Map FHandle File
  , next  :: FHandle
  }
  deriving Show

emptyFakeFS :: FakeFS
emptyFakeFS = F (Set.singleton (Dir [])) Map.empty Map.empty (Var 0)

type FakeOp a = FakeFS -> (Either Err a, FakeFS)

mMkDir :: Dir -> FakeOp ()
mMkDir d m@(F ds fs hs n)
  | d        `Set.member`    ds = (Left AlreadyExists, m)
  | parent d `Set.notMember` ds = (Left DoesNotExist, m)
  | otherwise                   = (Right (), F (Set.insert d ds) fs hs n)

mOpen :: File -> FakeOp FHandle
mOpen f m@(F ds fs hs n@(Var n_))
  | alreadyOpen   = (Left Busy, m)
  | not dirExists = (Left DoesNotExist, m)
  | fileExists    = (Right n, F ds fs hs' n')
  | otherwise     = (Right n, F ds (Map.insert f "" fs) hs' n')
  where
    hs' = Map.insert n f hs
    n'  = Var (succ n_)

    fileExists  =         f `Map.member` fs
    dirExists   = fileDir f `Set.member` ds
    alreadyOpen = f `List.elem` Map.elems hs

mWrite :: FHandle -> String -> FakeOp ()
mWrite h s m@(F ds fs hs n)
  | Just f <- Map.lookup h hs = (Right (), F ds (Map.adjust (++ s) f fs) hs n)
  | otherwise                 = (Left HandleClosed, m)

mClose :: FHandle -> FakeOp ()
mClose h (F ds fs hs n) = (Right (), F ds fs (Map.delete h hs) n)

mRead :: File -> FakeOp String
mRead f m@(F _ fs hs _)
  | alreadyOpen               = (Left Busy         , m)
  | Just s <- Map.lookup f fs = (Right s           , m)
  | otherwise                 = (Left DoesNotExist , m)
  where
    alreadyOpen = f `List.elem` Map.elems hs
