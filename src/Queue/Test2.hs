{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Queue.Test2 where

import Data.Map (Map)
import qualified Data.Map as Map
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Queue.FFI
import Stateful

------------------------------------------------------------------------

type State = Map (Var Queue) FQueue

data FQueue = FQueue
  { fqElems :: [Int]
  , fqSize  :: Int
  }
  deriving Show

fnew :: Int -> State -> Return Err (State, Var (Queue))
fnew sz s =
  let
    v = Var (Map.size s)
  in
    return (Map.insert v (FQueue [] sz) s, v)

fput :: Var Queue -> Int -> State -> Return Err (State, ())
fput q i s
  | q `Map.notMember` s = Precondition QueueDoesNotExist
  | length (fqElems (s Map.! q)) >= fqSize (s Map.! q) = Precondition QueueIsFull
  | otherwise = return (Map.adjust (\fq -> fq { fqElems = fqElems fq ++ [i] }) q s, ())

fget :: Var Queue -> State -> Return Err (State, Int)
fget q s
  | q `Map.notMember` s        = Precondition QueueDoesNotExist
  | null (fqElems (s Map.! q)) = Precondition QueueIsEmpty
  | otherwise = case fqElems (s Map.! q) of
      [] -> error "fget: impossible, we checked that it's non-empty"
      i : is -> return (Map.adjust (\fq -> fq { fqElems = is }) q s, i)

fsize :: Var Queue -> State -> Return Err (State, Int)
fsize q s
  | q `Map.notMember` s = Precondition QueueDoesNotExist
  | otherwise           = return (s, length (fqElems (s Map.! q)))

------------------------------------------------------------------------

data Err = QueueDoesNotExist | QueueIsFull | QueueIsEmpty
  deriving (Eq, Show)

instance StateModel State where

  initialState = Map.empty

  type Reference State = Queue
  type Failure State = Err

  data Command State q
    = New Int
    | Put q Int
    | Get q
    | Size q
    deriving (Show, Functor)

  data Response State q
    = New_ q
    | Put_ ()
    | Get_ Int
    | Size_ Int
    deriving (Eq, Show, Functor, Foldable)

  generateCommand s
    | Map.null s = New . getPositive <$> arbitrary
    | otherwise  = oneof
      [ New . getPositive <$> arbitrary
      , Put  <$> arbitraryQueue <*> arbitrary
      , Get  <$> arbitraryQueue
      , Size <$> arbitraryQueue
      ]
    where
      arbitraryQueue :: Gen (Var Queue)
      arbitraryQueue = Var <$> choose (0, Map.size s - 1)

  shrinkCommand _s (Put q i) = [ Put q i' | i' <- shrink i ]
  shrinkCommand _s _cmd = []

  runFake (New sz)  s = fmap New_  <$> fnew sz s
  runFake (Put q i) s = fmap Put_  <$> fput q i s
  runFake (Get q)   s = fmap Get_  <$> fget q s
  runFake (Size q)  s = fmap Size_ <$> fsize q s

  runReal (New sz)  = New_  <$> new sz
  runReal (Put q i) = Put_  <$> put q i
  runReal (Get q)   = Get_  <$> get q
  runReal (Size q)  = Size_ <$> size q

  runCommandMonad _ = id

prop_ok :: Commands State -> Property
prop_ok cmds = monadicIO $ do
  _ <- runCommands cmds
  assert True
