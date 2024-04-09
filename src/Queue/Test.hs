{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}

module Queue.Test where

import Control.Exception (SomeException, throwIO)
import Data.Dynamic
import Data.IntMap (IntMap)
import qualified Data.IntMap as Map
import Data.IORef
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Queue.Real
import Queue.Fake
import StateModel

------------------------------------------------------------------------

type State = IntMap SomeMQueue

type SomeMQueue = MQueue Dynamic

emptyState :: State
emptyState = Map.empty

lookupQueue :: Typeable a => State -> Int -> MQueue a
lookupQueue s i = fmap (flip fromDyn (error "lookupQueue")) (s Map.! i)

updateQueue :: Typeable a => Var (Queue a) -> (MQueue a -> Either Err (b, MQueue a)) -> FakeOp b
updateQueue (Var i) f s = case f (lookupQueue s i) of
  Left err      -> Left err
  Right (y, q') -> Right (Map.insert i (fmap toDyn q') s, y)

insertQueue :: Typeable a => MQueue a -> State -> (State, Var (Queue a))
insertQueue q s =
  let
    i = Map.size s
  in
    (Map.insert i (fmap toDyn q) s, Var i)

type FakeOp a = State -> Either Err (State, a)

fNew :: Typeable a => Int -> FakeOp (Var (Queue a))
fNew n s = do
  mq <- mNew n
  let (s', v) = insertQueue mq s
  return (s', v)

fPut :: Typeable a => a -> Var (Queue a) -> FakeOp ()
fPut x q = updateQueue q (mPut x)

fGet :: Typeable a => Var (Queue a) -> FakeOp a
fGet q = updateQueue q mGet

fSize :: Typeable a => Var (Queue a) -> FakeOp Int
fSize q = updateQueue q mSize

------------------------------------------------------------------------

data QueueI q = QueueI
  { iNew  :: forall a. Typeable a => Int -> IO (q a)
  , iPut  :: forall a. Typeable a => a -> q a -> IO ()
  , iGet  :: forall a. Typeable a => q a -> IO a
  , iSize :: forall a. Typeable a => q a -> IO Int
  }

data Compose g f x = Compose (g (f x))

real :: QueueI Queue
real = QueueI new put get size

runOp :: IORef State -> FakeOp a -> IO a
runOp ref op = do
  ex <- atomicModifyIORef' ref (\s -> case op s of
                                   Left err      -> (s,  Left err)
                                   Right (s', x) -> (s', Right x))
  x <- either throwIO return ex
  return x

fake :: IO (QueueI (Compose Var Queue))
fake = do
  qs <- newIORef emptyState :: IO (IORef State)
  let
    fNewIO :: forall a. Typeable a => Int -> IO (Compose Var Queue a)
    fNewIO n = fmap Compose $ runOp qs (fNew n)

    fPutIO :: Typeable a => a -> Compose Var Queue a -> IO ()
    fPutIO x (Compose q) = runOp qs (fPut x q)

    fGetIO :: Typeable a => Compose Var Queue a -> IO a
    fGetIO (Compose q) = runOp qs (fGet q)

    fSizeIO :: forall a. Typeable a => Compose Var Queue a -> IO Int
    fSizeIO (Compose q) = runOp qs (fSize q)

  return (QueueI fNewIO fPutIO fGetIO fSizeIO)

prog :: QueueI q -> IO ()
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
  prog real
  prog =<< fake

arbitraryVar :: Typeable a => State -> Gen (Var a)
arbitraryVar s = do
  i <- choose (0, Map.size s - 1)
  return (Var i)

------------------------------------------------------------------------

instance StateModel State where

  data Command State q a where
    New  :: Int -> Command State q q
    Put  :: Int -> q -> Command State q ()
    Get  :: q -> Command State q Int
    Size :: q -> Command State q Int

  type Reference State = Queue Int
  type Failure State = Err

  generateCommand :: State -> Gen (Untyped (Command State (Var (Queue Int))))
  generateCommand s
    | Map.null s = Untyped <$> (New <$> arbitrary)
    | otherwise  =
        frequency
          [ (3, Untyped <$> (Put <$> arbitrary <*> arbitraryVar s))
          , (5, Untyped <$> (Get <$> arbitraryVar s))
          , (1, Untyped <$> (New <$> arbitrary))
          ]

  shrinkCommand :: State -> Untyped (Command State q) -> [Untyped (Command State q)]
  shrinkCommand _s (Untyped (New n))   = map (Untyped . New) (shrink n)
  shrinkCommand _s (Untyped (Put x q)) = map (Untyped . flip Put q) (shrink x)
  shrinkCommand _s _         = []

  initialState :: State
  initialState = emptyState

  runFake :: Command State (Var (Reference State)) resp -> State
          -> Either (Failure State) (State, resp)
  runFake (New n)   = fNew n
  runFake (Put x q) = fPut x q
  runFake (Get q)   = fGet q
  runFake (Size q)  = fSize q

  runReal :: Env State -> Command State (Var (Reference State)) resp
          -> CommandMonad State (Return State resp)
  runReal _env (New n)   = Reference <$> new n
  runReal env  (Put x q) = Response  <$> put x (env q)
  runReal env  (Get q)   = Response  <$> get (env q)
  runReal env  (Size q)  = Response  <$> size (env q)

  abstractFailure :: State -> SomeException -> Maybe (Failure State)
  abstractFailure = undefined

  runCommandMonad _ = id

deriving instance Show q => Show (Command State q a)

prop_ok :: Commands State -> Property
prop_ok cmds = monadicIO $ do
  _ <- runCommands cmds
  assert True
