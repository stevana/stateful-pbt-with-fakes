{-# LANGUAGE CApiFFI #-}

module Queue.FFI where

import Foreign
import Foreign.C.Types

------------------------------------------------------------------------

data CQueue = CQueue
  { _buf  :: Ptr CInt
  , _inp  :: CInt
  , _outp :: CInt
  , _size :: CInt
  }

instance Storable CQueue where
  alignment _ = 8
  sizeOf _    = 20

  peek ptr = CQueue
    <$> peekByteOff ptr 0
    <*> peekByteOff ptr 8
    <*> peekByteOff ptr 12
    <*> peekByteOff ptr 16

  poke ptr (CQueue b i o s) = do
    pokeByteOff ptr 0 b
    pokeByteOff ptr 8 i
    pokeByteOff ptr 12 o
    pokeByteOff ptr 16 s

foreign import capi "queue.h new"  new_  :: CInt -> IO (Ptr CQueue)
foreign import capi "queue.h put"  put_  :: Ptr CQueue -> CInt -> IO ()
foreign import capi "queue.h get"  get_  :: Ptr CQueue -> IO CInt
foreign import capi "queue.h size" size_ :: Ptr CQueue -> IO CInt

foreign import capi "queue.h newBroken"  newBroken_  :: CInt -> IO (Ptr CQueue)
foreign import capi "queue.h sizeBroken" sizeBroken_ :: Ptr CQueue -> IO CInt

------------------------------------------------------------------------

newtype Queue = Queue (ForeignPtr CQueue)
  deriving (Eq, Show)

new :: Int -> IO Queue
new sz = do
  ptr <- new_ (fromIntegral sz)
  fptr <- newForeignPtr finalizerFree ptr
  return (Queue fptr)

newBroken :: Int -> IO Queue
newBroken sz = do
  ptr <- newBroken_ (fromIntegral sz)
  fptr <- newForeignPtr finalizerFree ptr
  return (Queue fptr)

put :: Queue -> Int -> IO ()
put (Queue fptr) n = withForeignPtr fptr $ \q ->
  put_ q (fromIntegral n)

get :: Queue -> IO Int
get (Queue fptr) = withForeignPtr fptr $ \q ->
  fromIntegral <$> get_ q

size :: Queue -> IO Int
size (Queue fptr) = withForeignPtr fptr $ \q ->
  fromIntegral <$> size_ q

sizeBroken :: Queue -> IO Int
sizeBroken (Queue fptr) = withForeignPtr fptr $ \q ->
  fromIntegral <$> sizeBroken_ q
