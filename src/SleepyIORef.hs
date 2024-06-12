-- start snippet SleepyIORef
module SleepyIORef (module SleepyIORef, IORef) where

import Control.Concurrent (threadDelay)
import Data.IORef (IORef)
import qualified Data.IORef as IORef

------------------------------------------------------------------------

readIORef :: IORef a -> IO a
readIORef ref = do
  IORef.readIORef ref
  threadDelay 100
-- end snippet SleepyIORef

newIORef :: a -> IO (IORef a)
newIORef = IORef.newIORef

atomicWriteIORef :: IORef a -> a -> IO ()
atomicWriteIORef = IORef.atomicWriteIORef

atomicModifyIORef' :: IORef a -> (a -> (a, b)) -> IO b
atomicModifyIORef' = IORef.atomicModifyIORef'
