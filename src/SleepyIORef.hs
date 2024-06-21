-- start snippet SleepyIORef
module SleepyIORef (module SleepyIORef, IORef) where

import Control.Concurrent (threadDelay)
import Data.IORef (IORef)
import qualified Data.IORef as IORef

readIORef :: IORef a -> IO a
readIORef ref = do
  threadDelay 1000
  IORef.readIORef ref
-- end snippet SleepyIORef

newIORef :: a -> IO (IORef a)
newIORef = IORef.newIORef

atomicWriteIORef :: IORef a -> a -> IO ()
atomicWriteIORef = IORef.atomicWriteIORef

atomicModifyIORef :: IORef a -> (a -> (a, b)) -> IO b
atomicModifyIORef ref f = do
  threadDelay 1000
  IORef.atomicModifyIORef ref f

atomicModifyIORef' :: IORef a -> (a -> (a, b)) -> IO b
atomicModifyIORef' ref f = do
  threadDelay 1000
  IORef.atomicModifyIORef' ref f
