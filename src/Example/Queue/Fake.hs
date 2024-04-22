module Example.Queue.Fake where

import Data.Map (Map)
import qualified Data.Map as Map

import Example.Queue.Real (Queue)
import Stateful (Var(..))

------------------------------------------------------------------------

type State = Map (Var Queue) FQueue

data FQueue = FQueue
  { fqElems :: [Int]
  , fqSize  :: Int
  }
  deriving Show

data Err = QueueDoesNotExist | QueueIsFull | QueueIsEmpty
  deriving (Eq, Show)

------------------------------------------------------------------------

fnew :: Int -> State -> Either Err (State, Var Queue)
fnew sz s =
  let
    v = Var (Map.size s)
  in
    return (Map.insert v (FQueue [] sz) s, v)

fput :: Var Queue -> Int -> State -> Either Err (State, ())
fput q i s
  | q `Map.notMember` s = Left QueueDoesNotExist
  | length (fqElems (s Map.! q)) >= fqSize (s Map.! q) = Left QueueIsFull
  | otherwise = return (Map.adjust (\fq -> fq { fqElems = fqElems fq ++ [i] }) q s, ())

fget :: Var Queue -> State -> Either Err (State, Int)
fget q s
  | q `Map.notMember` s        = Left QueueDoesNotExist
  | null (fqElems (s Map.! q)) = Left QueueIsEmpty
  | otherwise = case fqElems (s Map.! q) of
      [] -> error "fget: impossible, we checked that it's non-empty"
      i : is -> return (Map.adjust (\fq -> fq { fqElems = is }) q s, i)

fsize :: Var Queue -> State -> Either Err (State, Int)
fsize q s
  | q `Map.notMember` s = Left QueueDoesNotExist
  | otherwise           = return (s, length (fqElems (s Map.! q)))
