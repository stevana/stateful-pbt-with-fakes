{-# LANGUAGE LambdaCase #-}

module Example.FileSystem.Interface where

import Control.Exception
import Data.IORef
import System.IO
import System.Directory

import Example.FileSystem.Fake

------------------------------------------------------------------------

data IFileSystem h = IFileSystem
  { iMkDir :: Dir -> IO ()
  , iOpen  :: File -> IO h
  , iWrite :: h -> String -> IO ()
  , iClose :: h -> IO ()
  , iRead  :: File -> IO String
  }

------------------------------------------------------------------------

updateIORef :: IORef FakeFS -> FakeOp a -> IO a
updateIORef ref op =
  atomicModifyIORef' ref (\mock -> swap (op mock)) >>= \case
    Left err -> throwIO err
    Right x  -> return x
  where
    swap (x, y) = (y, x)

fake :: IO (IFileSystem FHandle)
fake = do
  ref <- newIORef emptyFakeFS
  return IFileSystem
    { iMkDir = \d   -> updateIORef ref (mMkDir d)
    , iOpen  = \f   -> updateIORef ref (mOpen f)
    , iWrite = \h s -> updateIORef ref (mWrite h s)
    , iClose = \h   -> updateIORef ref (mClose h)
    , iRead  = \f   -> updateIORef ref (mRead f)
    }

real :: FilePath -> IFileSystem Handle
real root = IFileSystem
  { iMkDir = \d   -> createDirectory (dirFP root d)
  , iOpen  = \f   -> openFile (fileFP root f) AppendMode
  , iWrite = \h s -> hPutStr h s
  , iClose = \h   -> hClose h
  , iRead  = \f   -> readFile (fileFP root f)
  }

------------------------------------------------------------------------

prog :: IFileSystem h -> IO ()
prog ifs = do
  let d = Dir ["foo"]
  iMkDir ifs d
  let f = File d "bar"
  h <- iOpen ifs f
  iWrite ifs h "baz"
  iClose ifs h
  putStrLn =<< iRead ifs f

unit_fake :: IO ()
unit_fake = prog =<< fake

unit_real :: IO ()
unit_real = prog (real "/tmp")
