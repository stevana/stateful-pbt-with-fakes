module Queue.Fake where

import Data.Map (Map)
import qualified Data.Map as Map

import Queue.Real (Queue)
import Stateful (Return(..), Var(..))

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

fnew :: Int -> State -> Return Err (State, Var Queue)
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
