module Example.Queue.Fake where

import Control.Exception
import Data.Map (Map)
import qualified Data.Map as Map

import Example.Queue.Real (Queue)
import Stateful (Var(..))

------------------------------------------------------------------------

type State = Map (Var Queue) FQueue

emptyState :: State
emptyState = Map.empty

data FQueue = FQueue
  { fqElems :: [Int]
  , fqSize  :: Int
  }
  deriving Show

data Err = QueueDoesNotExist | QueueIsFull | QueueIsEmpty
  deriving (Eq, Show)

instance Exception Err

------------------------------------------------------------------------

type FakeOp a = State -> Either Err (State, a)

fNew :: Int -> FakeOp (Var Queue)
fNew sz s =
  let
    v = Var (Map.size s)
  in
    return (Map.insert v (FQueue [] sz) s, v)

fPut :: Var Queue -> Int -> FakeOp ()
fPut q i s
  | q `Map.notMember` s = Left QueueDoesNotExist
  | length (fqElems (s Map.! q)) >= fqSize (s Map.! q) = Left QueueIsFull
  | otherwise = return (Map.adjust (\fq -> fq { fqElems = fqElems fq ++ [i] }) q s, ())

fGet :: Var Queue -> FakeOp Int
fGet q s
  | q `Map.notMember` s        = Left QueueDoesNotExist
  | null (fqElems (s Map.! q)) = Left QueueIsEmpty
  | otherwise = case fqElems (s Map.! q) of
      [] -> error "fget: impossible, we checked that it's non-empty"
      i : is -> return (Map.adjust (\fq -> fq { fqElems = is }) q s, i)

fSize :: Var Queue -> FakeOp Int
fSize q s
  | q `Map.notMember` s = Left QueueDoesNotExist
  | otherwise           = return (s, length (fqElems (s Map.! q)))
