{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}

module DieHard where

import Data.Void
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Stateful

------------------------------------------------------------------------

data Model = Model
  { bigJug   :: Int
  , smallJug :: Int
  }
  deriving (Eq, Show)

data BigJugIs4 = BigJugIs4
  deriving (Eq, Show)

instance StateModel Model where

  initialState = Model 0 0

  type Reference Model = Void
  type Failure Model = BigJugIs4

  data Command Model r
    = FillBig
    | FillSmall
    | EmptyBig
    | EmptySmall
    | SmallIntoBig
    | BigIntoSmall
    deriving (Show, Enum, Bounded, Functor)

  data Response Model r = Done
    deriving (Eq, Show, Functor, Foldable)

  generateCommand :: Model -> Gen (Command Model r)
  generateCommand _s = elements [minBound ..]

  runFake :: Command Model r -> Model -> Return (Failure Model) (Model, Response Model r)
  runFake FillBig      s = done s { bigJug   = 5 }
  runFake FillSmall    s = done s { smallJug = 3 }
  runFake EmptyBig     s = done s { bigJug   = 0 }
  runFake EmptySmall   s = done s { smallJug = 0 }
  runFake SmallIntoBig (Model big small) =
    let big' = min 5 (big + small) in
    done (Model { bigJug = big'
                , smallJug = small - (big' - big) })
  runFake BigIntoSmall (Model big small) =
    let small' = min 3 (big + small) in
    done (Model { bigJug = big - (small' - small)
                , smallJug = small'
                })

  runReal :: Concrete Model -> IO (Response Model (Reference Model))
  runReal _cmd = return Done

  monitoring :: (Model, Model) -> Concrete Model -> Response Model (Reference Model)
             -> Property -> Property
  monitoring (_s, s') _cmd _resp =
    counterexample $ "\n    State: "++show s'++"\n"

  runCommandMonad _s = id

done :: Model -> Return (Failure Model) (Model, Response Model ref)
done s' | bigJug s' == 4 = Throw BigJugIs4
        | otherwise      = Ok (s', Done)

prop_dieHard :: Commands Model -> Property
prop_dieHard cmds = withMaxSuccess 10000 $ monadicIO $ do
  _ <- runCommands cmds
  assert True
