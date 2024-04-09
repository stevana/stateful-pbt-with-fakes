{-# LANGUAGE DeriveFunctor #-}

module Queue.Fake where

import Control.Exception

------------------------------------------------------------------------

type MockOp a = Either Err a

data Err = NegativeSize Int | NoSpace | Empty
  deriving (Show, Eq)

instance Exception Err

data MQueue a = MQueue
  { capacity :: Int
  , content  :: [a]
  }
  deriving Functor

mNew :: Int -> MockOp (MQueue a)
mNew n | n >= 0    = Right (MQueue n [])
       | otherwise = Left (NegativeSize n)

mPut :: a -> MQueue a -> MockOp ((), MQueue a)
mPut x q | length (content q) < capacity q = Right ((), q { content = content q ++ [x] })
         | otherwise = Left NoSpace

mGet :: MQueue a -> MockOp (a, MQueue a)
mGet q = case content q of
  []       -> Left Empty
  (x : xs) -> Right (x, q { content = xs })

mSize :: MQueue a -> MockOp (Int, MQueue a)
mSize q = Right (length (content q), q)
