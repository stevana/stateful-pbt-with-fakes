module Example.CRUD.Real where

import Control.Concurrent
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import System.Directory
import System.IO
import System.IO.Error
import System.IO.Unsafe

------------------------------------------------------------------------

tEST_DIR :: FilePath
tEST_DIR = "/tmp/crud-test"

create :: IO (FilePath, Handle)
create = do
  createDirectoryIfMissing False tEST_DIR
  (fp, h) <- openTempFile tEST_DIR "crud.test"
  hSetBuffering h LineBuffering
  return (fp, h)

read' :: (FilePath, Handle) -> IO [String]
read' (_fp, h) = withLock $ do
  b <- hIsClosed h
  if b
  then return []
  else do
    pos <- hTell h
    hSeek h AbsoluteSeek 0
    ss <- go []
    hSeek h AbsoluteSeek pos
    return ss
  where
    go acc = do
      e <- tryIOError (hGetLine h)
      case e of
        Left err | isEOFError err -> return (reverse acc)
                 | otherwise -> error (show err)
        Right str -> go (str : acc)

update :: (FilePath, Handle) -> String -> IO ()
update (_fp, h) s = withLock $ do
  hPutStrLn h s

delete :: (FilePath, Handle) -> IO ()
delete (fp, h) = do
  hClose h
  removeFile fp

{-# NOINLINE lock #-}
lock :: MVar ()
lock = unsafePerformIO (newMVar ())

withLock :: IO a -> IO a
withLock io = do
  withMVar lock $ \_ -> io

cleanup :: IO ()
cleanup = removePathForcibly tEST_DIR
