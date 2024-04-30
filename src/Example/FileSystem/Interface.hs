{-# LANGUAGE LambdaCase #-}

module Example.FileSystem.Interface where

import Control.Exception
import Data.IORef
import System.IO

import Example.FileSystem.Fake
import Example.FileSystem.Real

------------------------------------------------------------------------

data IFileSystem h = IFileSystem
  { iMkDir :: Dir -> IO ()
  , iOpen  :: File -> IO h
  , iWrite :: h -> String -> IO ()
  , iClose :: h -> IO ()
  , iRead  :: File -> IO String
  }

------------------------------------------------------------------------

real :: IFileSystem Handle
real = IFileSystem
  { iMkDir = rMkDir
  , iOpen  = rOpen
  , iWrite = rWrite
  , iClose = rClose
  , iRead  = rRead
  }

updateIORef :: IORef FakeFS -> FakeOp a -> IO a
updateIORef ref op =
  atomicModifyIORef' ref (\fs -> swap (op fs)) >>= \case
    Left err -> throwIO err
    Right x  -> return x
  where
    swap (x, y) = (y, x)

fake :: IO (IFileSystem FHandle)
fake = do
  ref <- newIORef emptyFakeFS
  return IFileSystem
    { iMkDir = \d   -> updateIORef ref (fMkDir d)
    , iOpen  = \f   -> updateIORef ref (fOpen f)
    , iWrite = \h s -> updateIORef ref (fWrite h s)
    , iClose = \h   -> updateIORef ref (fClose h)
    , iRead  = \f   -> updateIORef ref (fRead f)
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

unit_real :: IO ()
unit_real = prog real

unit_fake :: IO ()
unit_fake = prog =<< fake
