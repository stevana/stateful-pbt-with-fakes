module AtomicCounter where

import Data.IORef

------------------------------------------------------------------------

newtype AtomicCounter = AtomicCounter (IORef Int)

newAtomicCounter :: IO AtomicCounter
newAtomicCounter = AtomicCounter <$> newIORef 0

incrAtomicCounter :: AtomicCounter -> Int -> IO Int
incrAtomicCounter (AtomicCounter ref) n =
  atomicModifyIORef' ref (\old -> (old + n, old))
