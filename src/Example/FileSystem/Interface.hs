{-# LANGUAGE LambdaCase #-}

module Example.FileSystem.Interface where

import Control.Exception
import Data.IORef
import System.IO

import Example.FileSystem.Fake
import Example.FileSystem.Real

------------------------------------------------------------------------

-- start snippet IFileSystem
data IFileSystem h = IFileSystem
  { iMkDir :: Dir -> IO ()
  , iOpen  :: File -> IO h
  , iWrite :: h -> String -> IO ()
  , iClose :: h -> IO ()
  , iRead  :: File -> IO String
  }
-- end snippet IFileSystem

------------------------------------------------------------------------

-- start snippet real
real :: IFileSystem Handle
real = IFileSystem
  { iMkDir = rMkDir
  , iOpen  = rOpen
  , iWrite = rWrite
  , iClose = rClose
  , iRead  = rRead
  }
-- end snippet real

-- start snippet fake
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
  where
    updateIORef :: IORef FakeFS -> FakeOp a -> IO a
    updateIORef ref op =
      atomicModifyIORef' ref (\fs -> swap (op fs)) >>= \case
        Left err -> throwIO err
        Right x  -> return x
      where
        swap (x, y) = (y, x)
-- end snippet fake

------------------------------------------------------------------------

-- start snippet prog
prog :: IFileSystem h -> IO ()
prog ifs = do
  let d = Dir ["foo"]
  iMkDir ifs d
  let f = File d "bar"
  h <- iOpen ifs f
  iWrite ifs h "baz"
  iClose ifs h
  putStrLn =<< iRead ifs f

test :: IO ()
test = prog =<< fake

deploy :: IO ()
deploy = prog real
-- end snippet prog
