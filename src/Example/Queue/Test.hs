{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Example.Queue.Test where

import qualified Data.Map as Map
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Example.Queue.Real
import Example.Queue.Fake
import Stateful

------------------------------------------------------------------------

instance StateModel State where

  initialState = Map.empty

  type Reference State = Queue

  type PreconditionFailure State = Err

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
      -- NOTE: Uncomment once precondition for put has been added and the property passes.
      , Size <$> arbitraryQueue
      ]
    where
      arbitraryQueue :: Gen (Var Queue)
      arbitraryQueue = elements (Map.keys s)

  shrinkCommand _s (New i)   = [ New i'   | Positive i' <- shrink (Positive i) ]
  shrinkCommand _s (Put q i) = [ Put q i' | i' <- shrink i ]
  shrinkCommand _s _cmd = []

  runFake (New sz)  s = fmap New_  <$> fNew sz s
  runFake (Put q i) s = fmap Put_  <$> fPut q i s
  runFake (Get q)   s = fmap Get_  <$> fGet q s
  runFake (Size q)  s = fmap Size_ <$> fSize q s

  runReal (New sz)  = New_  <$> new sz
  runReal (Put q i) = Put_  <$> put q i
  runReal (Get q)   = Get_  <$> get q
  runReal (Size q)  = Size_ <$> size q

  runCommandMonad _ = id

prop_queue :: Commands State -> Property
prop_queue cmds = monadicIO $ do
  runCommands cmds
  assert True

unit_queueFull :: IO ()
unit_queueFull = quickCheck (withMaxSuccess 1 (prop_queue cmds))
  where
    cmds = Commands
      [ New 1
      , Put (Var 0) 1
      , Put (Var 0) 0
      , Get (Var 0)
      ]

unit_queueSize :: IO ()
unit_queueSize = quickCheck (withMaxSuccess 1 (prop_queue cmds))
  where
    cmds = Commands
      [ New 1
      , Put (Var 0) 0
      , Size (Var 0)
      ]
