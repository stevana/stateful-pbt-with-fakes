{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Example.KeyValueStore.Real where

import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map

import Opaque

------------------------------------------------------------------------

newtype Store v = Store (Opaque (IORef (Map Key v)))
  deriving (Eq, Show)

type Key = String

------------------------------------------------------------------------

newStore :: IO (Store v)
newStore = Store . Opaque <$> newIORef Map.empty

writeKV :: Store v -> Key -> v -> IO ()
writeKV (Store (Opaque ref)) key value =
  atomicModifyIORef' ref (\m -> (Map.insert key value m, ()))

readKV :: Store v -> Key -> IO (Maybe v)
readKV (Store (Opaque ref)) key = do
  m <- readIORef ref
  return (Map.lookup key m)

deleteKV :: Store v -> Key -> IO ()
deleteKV (Store (Opaque ref)) key =
  atomicModifyIORef' ref (\m -> (Map.delete key m, ()))
