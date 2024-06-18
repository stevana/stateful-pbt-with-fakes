{-# LANGUAGE LambdaCase #-}

module Example.Queue.Interface where

import Control.Exception
import Data.IORef

import Example.Queue.Fake
import Example.Queue.Real
import Stateful (Var(..))

------------------------------------------------------------------------

-- start snippet IQueue
data IQueue q = IQueue
  { iNew  :: Int -> IO q
  , iPut  :: q -> Int -> IO ()
  , iGet  :: q -> IO Int
  , iSize :: q -> IO Int
  }
-- end snippet IQueue

-- start snippet real
real :: IQueue Queue
real = IQueue
  { iNew  = new
  , iPut  = put
  , iGet  = get
  , iSize = size
  }
-- end snippet real

-- start snippet fake
fake :: IO (IQueue (Var Queue))
fake = do
  ref <- newIORef emptyState
  return IQueue
    { iNew  = \n   -> updateIORef ref (fNew n)
    , iPut  = \q i -> updateIORef ref (fPut q i)
    , iGet  = \q   -> updateIORef ref (fGet q)
    , iSize = \q   -> updateIORef ref (fSize q)
    }
  where
    updateIORef :: IORef State -> FakeOp a -> IO a
    updateIORef ref op =
      atomicModifyIORef' ref (\fs -> assoc fs (op fs)) >>= \case
        Left err -> throwIO err
        Right x  -> return x
      where
        assoc fs  (Left err)       = (fs,  Left err)
        assoc _fs (Right (fs', x)) = (fs', Right x)
-- end snippet fake

-- start snippet prog
prog :: IQueue q -> IO ()
prog iq = do
  q <- iNew iq 3
  iPut iq q 0
  iPut iq q 1
  iPut iq q 2
  x <- iGet iq q
  assert (x == 0) (return ())
  sz <- iSize iq q
  assert (sz == 2) (return ())
-- end snippet prog

unit_real :: IO ()
unit_real = prog real

unit_fake :: IO ()
unit_fake = prog =<< fake
