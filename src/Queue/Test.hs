{-# LANGUAGE DeriveFunctor            #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE Rank2Types               #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications         #-}

module Queue.Test where

-- https://github.com/nick8325/quickcheck/issues/139

import           Control.Exception
import           Data.Array.IO
import           Data.Dynamic
import           Data.Either
import           Data.IntMap          (IntMap)
import qualified Data.IntMap          as Map
import           Data.IORef
import           Data.Kind
import           Data.Typeable
import           Test.QuickCheck
import           Test.QuickCheck.Poly

------------------------------------------------------------------------
-- Bounded queues, without any error checking.

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

------------------------------------------------------------------------

type State = IntMap SomeMQueue

type SomeMQueue = MQueue Dynamic

initialState :: State
initialState = Map.empty

lookupQueue :: Typeable a => Var (Queue a) -> State -> MQueue a
lookupQueue (Var i) m = fmap (flip fromDyn (error "lookupQueue")) (m Map.! i)

updateQueue :: Typeable a => Var (Queue a) -> (MQueue a -> Either Err (b, MQueue a)) -> FakeOp b
updateQueue (Var i) f s = undefined
  -- Map.adjust (fmap toDyn . f . fmap (flip fromDyn (error "updateQueue"))) i s

insertQueue :: Typeable a => MQueue a -> State -> (State, Var (Queue a))
insertQueue q s =
  let
    i = Map.size s
  in
    (Map.insert i (fmap toDyn q) s, Var i)

type FakeOp a = State -> Either Err (State, Either (Var a) a)

fakeOp :: MockOp a -> FakeOp a
fakeOp f s = undefined


fNew :: Typeable a => Int -> FakeOp (Queue a)
fNew n s = do
  mq <- mNew n
  let (s', v ) = insertQueue mq s
  return (s', Left v)

fPut :: Typeable a => a -> Var (Queue a) -> FakeOp ()
fPut x q s = updateQueue q (mPut x) s
  {-
      m <- readIORef qs
      let q = lookupMQueue i m
      (r, q') <- runOp (mPut x q)
      let m' = insertMQueue i q' m
      writeIORef qs m'
      return r
-}

fGet = undefined


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


lookupMQueue :: Typeable a => Int -> IntMap SomeMQueue -> MQueue a
lookupMQueue i m = fmap (flip fromDyn (error "lookupMQueue")) (m Map.! i)

insertMQueue :: Typeable a => Int -> MQueue a -> IntMap SomeMQueue -> IntMap SomeMQueue
insertMQueue i q m = Map.insert i (fmap toDyn q) m

fake :: IO (QueueI (Compose Var Queue))
fake = do
  qs <- newIORef initialState :: IO (IORef State)
  let
    fNewIO :: forall a. Typeable a => Int -> IO (Compose Var Queue a)
    fNewIO n = undefined -- atomicModifyIORef' qs (fmap Compose . fNew n)

    fPutIO :: Typeable a => a -> Compose Var Queue a -> IO ()
    fPutIO x (Compose q) = undefined -- atomicModifyIORef' qs (fPut x q)

    fGet :: Typeable a => Compose Var Queue a -> IO a
    fGet (Compose (Var i)) = do
      m <- readIORef qs
      let q = lookupMQueue i m
      (r, q') <- runOp (mGet q)
      let m' = insertMQueue i q' m
      writeIORef qs m'
      return r

    fSize :: forall a. Typeable a => Compose Var Queue a -> IO Int
    fSize (Compose (Var i)) = do
      m <- readIORef qs
      let q = lookupMQueue i m :: MQueue a
      (r, q') <- runOp (mSize q)
      let m' = insertMQueue i (fmap toDyn q') m
      writeIORef qs m'
      return r

  return (QueueI fNewIO fPutIO fGet fSize)

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

runFake :: forall a. State -> Command a -> Either Err (State, Either (Var a) a)
runFake s (New n)   = fNew n s
runFake s (Put x q) = undefined
runFake s (Get q)   = do
  let mq :: MQueue a
      mq = lookupQueue q s
  (x, _q') <- mGet mq
  return (s, Right x)
runFake s (Size q)  = undefined

------------------------------------------------------------------------

data Command a where
  New  :: Int -> Command (Queue A)
  Put  :: A -> Var (Queue A) -> Command ()
  Get  :: Var (Queue A) -> Command A
  Size :: Var (Queue A) -> Command Int
deriving instance Show (Command a)

runCommand :: Env -> Command a -> IO a
runCommand _ (New n)       = new n
runCommand env (Put x ref) = put x (env ref)
runCommand env (Get ref)   = get (env ref)
runCommand env (Size ref)  = size (env ref)

shrinkCommand :: Command a -> [Command a]
shrinkCommand (New n)   = map New (shrink n)
shrinkCommand (Put x q) = map (flip Put q) (shrink x)
shrinkCommand _         = []

pre :: Command a -> State -> Bool
pre cmd s = isRight (runFake s cmd)

post :: (Eq a, Show a) => Command a -> a -> State -> Property
post cmd resp s = case runFake s cmd of
  Left err -> counterexample (show err) (property False)
  Right (_s', Right resp') -> resp === resp'
  Right (_s', Left _)      -> property True -- XXX?

nextState :: Command a -> Var a -> State -> State
nextState cmd q s = case runFake s cmd of
  Left err -> error ("nextState: " ++ show err)
  Right (s', _) -> s'

genCommand :: State -> Gen (Untyped Command)
genCommand s
  | Map.null s = Untyped <$> (New <$> arbitrary)
  | otherwise  =
      frequency
        [ (3, Untyped <$> (Put <$> arbitrary <*> arbitraryQueue s))
        , (5, Untyped <$> (Get <$> arbitraryQueue s))
        , (1, Untyped <$> (New <$> arbitrary))
        ]

arbitraryQueue :: Typeable a => State -> Gen (Var (Queue a))
arbitraryQueue s = do
  i <- choose (0, Map.size s - 1)
  return (Var i)


{-
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

-}
prop_ok cmds = runCommands cmds

------------------------------------------------------------------------
-- Testing, reusable code.

data Var a = Var Int deriving Show
type Env = forall a. Typeable a => Var a -> a

newtype Commands = Commands [Cmd] deriving Show
data Cmd where
  -- A command together with its result
  Cmd :: forall a. (Typeable a, Eq a, Show a) => Command a -> Var a -> Cmd
deriving instance Show Cmd

data Untyped f where
  Untyped :: (Typeable a, Eq a, Show a) => f a -> Untyped f

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
          post cmd res state .&&.
            let next = nextState cmd var state in
            runCmds next ((x, toDyn res):vals) cmds

    sub :: Typeable a => [(Int, Dynamic)] -> Var a -> a
    sub vals (Var x) =
      case lookup x vals of
        Nothing -> discard
          -- ^ this can happen if a shrink step makes a variable unbound
        Just val ->
          case fromDynamic val of
            Nothing  -> error $ "variable " ++ show x ++ " has wrong type"
            Just val -> val
