{-# LANGUAGE GADTs, StandaloneDeriving, Rank2Types, RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MyLib where

import Control.Exception
import Test.QuickCheck
import Test.QuickCheck.Poly
import Data.IORef
import Data.Array.IO
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Dynamic
import Data.Typeable

------------------------------------------------------------------------
-- Bounded queues, without any error checking.

data Queue a =
  Queue {
    queue_array :: IOArray Int a,
    queue_peek  :: IORef Int,
    queue_poke  :: IORef Int }

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

------------------------------------------------------------------------
-- Mock

type MockOp a = Either Err a

data Err = NegativeSize Int | NoSpace | Empty
  deriving Show

instance Exception Err

data MQueue a = MQueue
  { capacity :: Int
  , content  :: [a]
  }

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

------------------------------------------------------------------------

data QueueI q = QueueI
  { iNew  :: forall a. Typeable a => Int -> IO (q a)
  , iPut  :: forall a. Typeable a => a -> q a -> IO ()
  , iGet  :: forall a. Typeable a => q a -> IO a
  , iSize :: forall a. Typeable a => q a -> IO Int
  }

data Compose g f x = Compose (g (f x))

real :: IO (QueueI Queue)
real = return (QueueI new put get size)

runOp :: MockOp a -> IO a
runOp op = either throwIO return op


data SomeMQueue = forall a. Typeable a => SomeMQueue (MQueue a)


fake :: IO (QueueI (Compose Var Queue))
fake = do
  qs <- newIORef Map.empty :: IO (IORef (Map Int SomeMQueue))
  let
    fNew :: forall a. Typeable a => Int -> IO (Compose Var Queue a)
    fNew n = atomicModifyIORef' qs (\m -> let i = Map.size m in (Map.insert i (SomeMQueue q) m, Compose (Var i)))
      where
        q :: MQueue a
        q = MQueue n []

    fPut :: Typeable a => a -> Compose Var Queue a -> IO ()
    fPut x (Compose (Var i)) = do
      m <- readIORef qs
      case m Map.! i of
        SomeMQueue q_ -> case cast q_ of
          Nothing -> error ""
          Just q -> do
            (r, q') <- runOp (mPut x q)
            let m' = Map.insert i (SomeMQueue q') m
            writeIORef qs m'
            return r

    fGet :: Typeable a => Compose Var Queue a -> IO a
    fGet (Compose (Var i)) = do
      m <- readIORef qs
      case m Map.! i of
        SomeMQueue q_ -> case cast q_ of
          Nothing -> error ""
          Just q -> do
            (r, q') <- runOp (mGet q)
            let m' = Map.insert i (SomeMQueue q') m
            writeIORef qs m'
            return r

    fSize :: forall a. Typeable a => Compose Var Queue a -> IO Int
    fSize (Compose (Var i)) = do
      m <- readIORef qs
      case m Map.! i of
        SomeMQueue q_ -> case cast q_ of
          Nothing -> error ""
          Just (q :: MQueue a) -> do
            (r, q') <- runOp (mSize q)
            let m' = Map.insert i (SomeMQueue q') m
            writeIORef qs m'
            return r

  return (QueueI fNew fPut fGet fSize)

prog :: forall q. QueueI q -> IO ()
prog i = do
  q <- iNew i 3
  iPut i 'a' q
  iPut i 'b' q
  a <- iGet i q
  sz <- iSize i q
  print (a, sz)
  q2 <- iNew i 2
  iPut i ("foo" :: String) q2
  iPut i "bar" q2
  foo <- iGet i q2
  bar <- iGet i q2
  print (foo, bar)

test :: IO ()
test = do
  real >>= prog
  fake >>= prog

------------------------------------------------------------------------

-- Testing, stuff specific to this example.

data State =
    NoQueue
  | HaveQueue {
      state_queue    :: Var (Queue A),
      state_capacity :: Int,
      state_contents :: [A] }

initialState :: State
initialState = NoQueue

data Command a where
  New  :: Int -> Command (Queue A)
  Put  :: A -> Var (Queue A) -> Command ()
  Get  :: Var (Queue A) -> Command A
  Size :: Var (Queue A) -> Command Int
deriving instance Show (Command a)

runCommand :: Env -> Command a -> IO a
runCommand _ (New n) = new n
runCommand env (Put x ref) = put x (env ref)
runCommand env (Get ref) = get (env ref)
runCommand env (Size ref) = size (env ref)

genCommand :: State -> Gen (Untyped Command)
genCommand NoQueue = fmap (Untyped . New) arbitrary
genCommand HaveQueue{..} =
  oneof [
    fmap (Untyped . flip Put state_queue) arbitrary,
    return (Untyped (Get state_queue)),
    return (Untyped (Size state_queue))]

shrinkCommand :: Command a -> [Command a]
shrinkCommand (New n) = map New (shrink n)
shrinkCommand (Put x q) = map (flip Put q) (shrink x)
shrinkCommand _ = []

pre :: Command a -> State -> Bool
pre New{} HaveQueue{} = False
pre (New n) NoQueue = n >= 0
pre Put{} HaveQueue{..} =
  length state_contents < state_capacity
pre Get{} HaveQueue{state_contents = []} = False
pre _ _ = True

post :: Env -> Command a -> a -> State -> Property
post _ Get{} x HaveQueue{state_contents = y:_} =
  counterexample "Wrong value returned from get" $
  x === y
post _ Size{} n HaveQueue{..} =
  counterexample "Wrong length returned from size" $
  n === length state_contents
post _ _ _ _ = property True

nextState :: Command a -> Var a -> State -> State
nextState (New n) q NoQueue =
  HaveQueue q n []
nextState (Put x q) _ state@HaveQueue{..} =
  state{state_contents = state_contents ++ [x]}
nextState (Get q) _ state@HaveQueue{state_contents = _:xs} =
  state{state_contents = xs}
nextState (Size _) _ s = s

prop_ok cmds = runCommands cmds

------------------------------------------------------------------------
-- Testing, reusable code.

data Var a = Var Int deriving Show
type Env = forall a. Typeable a => Var a -> a

newtype Commands = Commands [Cmd] deriving Show
data Cmd where
  -- A command together with its result
  Cmd :: forall a. Typeable a => Command a -> Var a -> Cmd
deriving instance Show Cmd

data Untyped f where
  Untyped :: Typeable a => f a -> Untyped f

instance Arbitrary Commands where
  arbitrary = Commands <$> do
    n <- sized $ \k -> choose (0, k)
    let
      -- First argument: number of commands to generate
      -- Second argument: next variable index to use
      loop :: Int -> Int -> State -> Gen [Cmd]
      loop 0 _ _ = return []
      loop n i state = do
        Untyped cmd <- genCommand state `suchThat` (\(Untyped cmd) -> pre cmd state)
        let res  = Var i
            next = nextState cmd res state
        fmap (Cmd cmd res:) (loop (n-1) (i+1) next)
    loop n 0 initialState

  shrink (Commands cmds) =
    map (Commands . prune initialState []) (shrinkList shrinkCmd cmds)
    where
      shrinkCmd (Cmd cmd x) =
        [ Cmd cmd' x | cmd' <- shrinkCommand cmd ]

      -- Remove any commands which don't satisfy their precondition
      prune :: State -> [Int] -> [Cmd] -> [Cmd]
      prune _ _ [] = []
      prune state bound (Cmd cmd x@(Var res):cmds)
        | pre cmd state =
          Cmd cmd x:
          prune (nextState cmd x state) (res:bound) cmds
        | otherwise = prune state bound cmds

runCommands :: Commands -> Property
runCommands (Commands cmds) = runCmds initialState [] cmds
  where
    runCmds :: State -> [(Int, Dynamic)] -> [Cmd] -> Property
    runCmds _ _ [] = property True
    runCmds state vals (Cmd cmd var@(Var x):cmds) =
      counterexample (show cmd) $ ioProperty $ do
        res <- runCommand (sub vals) cmd
        return $
          post (sub vals) cmd res state .&&.
            let next = nextState cmd var state in
            runCmds next ((x, toDyn res):vals) cmds

    sub :: Typeable a => [(Int, Dynamic)] -> Var a -> a
    sub vals (Var x) =
      case lookup x vals of
        Nothing -> discard
          -- ^ this can happen if a shrink step makes a variable unbound
        Just val ->
          case fromDynamic val of
            Nothing -> error $ "variable " ++ show x ++ " has wrong type"
            Just val -> val
