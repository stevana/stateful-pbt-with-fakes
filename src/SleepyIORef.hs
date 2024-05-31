module SleepyIORef (module SleepyIORef, IORef) where

import Control.Concurrent (threadDelay)
import Data.IORef (IORef)
import qualified Data.IORef as IORef

------------------------------------------------------------------------

newIORef :: a -> IO (IORef a)
newIORef = IORef.newIORef

readIORef :: IORef a -> IO a
readIORef ref = do
  threadDelay 2000
  IORef.readIORef ref

atomicWriteIORef :: IORef a -> a -> IO ()
atomicWriteIORef = IORef.atomicWriteIORef

atomicModifyIORef' :: IORef a -> (a -> (a, b)) -> IO b
atomicModifyIORef' = IORef.atomicModifyIORef'
