{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}

module Example.CRUD.Test where

import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import System.IO
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Example.CRUD.Real
import Parallel
import Stateful

------------------------------------------------------------------------

data State = State
  { store :: Map (Var (Reference State)) [String]
  , next  :: Int
  }
  deriving (Eq, Ord, Show)

fCreate :: State -> (State, Var (Reference State))
fCreate s = (s { store = Map.insert ref [] (store s)
               , next  = next s + 1
               }, ref)
  where
    ref = Var (next s)

fRead :: State -> Var (Reference State) -> Either () (State, [String])
fRead s@(State m _) ref | Map.member ref m = case Map.lookup ref m of
  Nothing -> Right (s, [])
  Just ss -> Right (s, ss)
                        | otherwise = Left ()

fUpdate :: State -> Var (Reference State) -> String -> Either () (State, ())
fUpdate (State m n) ref v
  | Map.member ref m = Right (State (Map.adjust (++ [v]) ref m) n, ())
  | otherwise        = Left ()

fDelete :: State -> Var (Reference State) -> Either () (State, ())
fDelete (State m n) ref
  | Map.member ref m = Right (State (Map.delete ref m) n, ())
  | otherwise        = Left ()

instance StateModel State where

  initialState = State Map.empty 0

  type Reference State = (FilePath, Handle)

  data Command State ref
    = Create
    | Read ref
    | Update ref String
    | Delete ref
    deriving (Show, Functor, Foldable)

  data Response State ref
    = Create_ ref
    | Read_ [String]
    | Update_ ()
    | Delete_ ()
    deriving (Eq, Show, Functor, Foldable)

  generateCommand s@(State m _n) = frequency $
    [ (1, pure Create) ] ++
    [ (5, Read   <$> arbitraryRef s) | not (Map.null m) ] ++
    [ (3, Update <$> arbitraryRef s <*> arbitraryLine) | not (Map.null m) ] ++
    [ (1, Delete <$> arbitraryRef s) | not (Map.null m) ]
    where
      arbitraryRef :: State -> Gen (Var (Reference State))
      arbitraryRef (State m0 _n) = elements (Map.keys m0)

      arbitraryLine :: Gen String
      arbitraryLine = do
        len <- choose (0, 5)
        vectorOf len (elements ['A'.. 'Z'])

  type PreconditionFailure State = ()

  runFake Create         s = return (fmap Create_ (fCreate s))
  runFake (Read ref)     s = fmap (fmap Read_)   (fRead s ref)
  runFake (Update ref v) s = fmap (fmap Update_) (fUpdate s ref v)
  runFake (Delete ref)   s = fmap (fmap Delete_) (fDelete s ref)

  runReal Create               = Create_ <$> create
  runReal (Read ref)           = Read_   <$> read' ref
  runReal (Update ref s)       = Update_ <$> update ref s
  runReal (Delete ref)         = Delete_ <$> delete ref

instance ParallelModel State where
  runCommandMonad _ = id

prop_crud :: Commands State -> Property
prop_crud cmds = monadicIO $ do
  runCommands cmds
  run cleanup
  assert True

prop_parallelCrud :: ParallelCommands State -> Property
prop_parallelCrud cmds = monadicIO $ do
  replicateM_ 10 (runParallelCommands cmds)
  run cleanup
  assert True
