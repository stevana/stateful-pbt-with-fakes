{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Queue.Test where

import qualified Data.Map as Map
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Queue.Real
import Queue.Fake
import Stateful

------------------------------------------------------------------------

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

prop_queue :: Commands State -> Property
prop_queue cmds = monadicIO $ do
  _ <- runCommands cmds
  assert True
