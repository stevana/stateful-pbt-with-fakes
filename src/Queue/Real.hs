{-# LANGUAGE RecordWildCards #-}

module Queue.Real where

import Data.Array.IO
import Data.IORef

------------------------------------------------------------------------
-- Bounded queues, without any error checking.
-- https://github.com/nick8325/quickcheck/issues/139

data Queue a =
  Queue {
    queue_array :: IOArray Int a,
    queue_peek  :: IORef Int,
    queue_poke  :: IORef Int }

-- XXX: can we avoid having to define Eq and Show?
instance Eq (Queue a) where
  (==) = undefined

instance Show (Queue a) where
  show = undefined

new :: Int -> IO (Queue a)
new n = do
  array <- newArray (0, n) undefined
  peek  <- newIORef 0
  poke  <- newIORef 0
  return (Queue array peek poke)

put :: a -> Queue a -> IO ()
put x Queue{..} = do
  poke <- readIORef queue_poke
  writeArray queue_array poke x
  n <- arraySize queue_array
  writeIORef queue_poke $! (poke + 1) `mod` n

get :: Queue a -> IO a
get Queue{..} = do
  peek <- readIORef queue_peek
  x <- readArray queue_array peek
  n <- arraySize queue_array
  writeIORef queue_peek $! (peek + 1) `mod` n
  return x

size :: Queue a -> IO Int
size Queue{..} = do
  peek <- readIORef queue_peek
  poke <- readIORef queue_poke
  n <- arraySize queue_array
  return ((poke-peek) `mod` n)

arraySize :: (Num i, Ix i) => IOArray i a -> IO i
arraySize array = do
  (lo, hi) <- getBounds array
  return (hi-lo+1)
