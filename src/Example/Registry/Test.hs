{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}

module Example.Registry.Test where

import Control.Arrow
import Control.Concurrent
import Control.Exception (ErrorCall(..), try)
import Control.Monad
import Data.Either
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Example.Registry.Real
import Parallel
import Stateful

------------------------------------------------------------------------

-- start snippet Registry
data RegState = RegState
  { tids   :: [Var ThreadId]
  , regs   :: [(String, Var (ThreadId))]
  , killed :: [Var ThreadId]
  }
  deriving (Eq, Ord, Show)

instance StateModel RegState where

  initialState :: RegState
  initialState = RegState [] [] []

  type Reference RegState = ThreadId

  data Command RegState tid
    = Spawn
    | WhereIs String
    | Register String tid
    | Unregister String
    | Kill tid
    deriving (Show, Functor, Foldable)

  data Response RegState tid
    = Spawn_ tid
    | WhereIs_ (NonFoldable (Maybe tid))
    | Register_ (Either ErrorCall ())
    | Unregister_ (Either ErrorCall ())
    | Kill_ ()
    deriving (Eq, Show, Functor, Foldable)

  generateCommand :: RegState -> Gen (Command RegState (Var ThreadId))
  generateCommand s = oneof $
    [ return Spawn ] ++
    [ Register <$> arbitraryName <*> elements (tids s) | not (null (tids s)) ] ++
    [ Unregister <$> arbitraryName
    , WhereIs <$> arbitraryName
    ] ++
    [ Kill <$> elements (tids s) | not (null (tids s)) ]
    where
      arbitraryName :: Gen String
      arbitraryName = elements allNames

  runFake :: Command RegState (Var ThreadId)-> RegState
          -> Either void (RegState, Response RegState (Var ThreadId))
  runFake Spawn               s = let tid = Var (length (tids s)) in
                                  return (s { tids = tids s ++ [tid] }, Spawn_ tid)
  runFake (WhereIs name)      s = return (s, WhereIs_ (NonFoldable (lookup name (regs s))))
  runFake (Register name tid) s
    | tid `elem` tids s
    , name `notElem` map fst (regs s)
    , tid `notElem` map snd (regs s)
    , tid `notElem` killed s
    = return (s { regs = (name, tid) : regs s }, Register_ (Right ()))

    | otherwise
    = return (s, Register_ (Left (ErrorCall "bad argument")))

  runFake (Unregister name)   s
    | name `elem` map fst (regs s) =
        return (s { regs = remove name (regs s) }, Unregister_ (Right ()))
    | otherwise = return (s, Unregister_ (Left (ErrorCall "bad argument")))
    where
      remove x = filter ((/= x) . fst)
  runFake (Kill tid) s = return (s { killed = tid : killed s
                                   , regs   = remove tid (regs s)}, Kill_ ())
    where
      remove x = filter ((/= x) . snd)

  runReal :: Command RegState ThreadId -> IO (Response RegState ThreadId)
  runReal Spawn               = Spawn_      <$> spawn
  runReal (WhereIs name)      = WhereIs_ . NonFoldable <$> whereis name
  runReal (Register name tid) = Register_   <$> fmap (left abstractError) (try (register name tid))
  runReal (Unregister name)   = Unregister_ <$> fmap (left abstractError) (try (unregister name))
  runReal (Kill tid)          = Kill_       <$> kill tid

  monitoring :: (RegState, RegState) -> Command RegState ThreadId -> Response RegState ThreadId
             -> Property -> Property
  monitoring (_s, _s') Register {} (Register_ resp) = classify (isLeft resp) (show RegisterFailed)
  monitoring (_s, s') _cmd _resp =
    counterexample $ "\n    State: " ++ show s' ++ "\n"

-- Throws away the location information from the error, so that it matches up
-- with the fake.
abstractError :: ErrorCall -> ErrorCall
abstractError (ErrorCallWithLocation msg _loc) = ErrorCall msg

allNames :: [String]
allNames = ["a", "b", "c", "d", "e"]

data Tag = RegisterFailed
  deriving Show

prop_registry :: Commands RegState -> Property
prop_registry cmds = monadicIO $ do
  runCommands cmds
  void (run cleanUp)
  assert True

cleanUp :: IO [Either ErrorCall ()]
cleanUp = sequence
  [ try (unregister name) :: IO (Either ErrorCall ())
  | name <- allNames
  ]
-- end snippet Registry

-- start snippet ParallelRegistry
instance ParallelModel RegState where
  runCommandMonad _ = id

prop_parallelRegistry :: ParallelCommands RegState -> Property
prop_parallelRegistry cmds = monadicIO $ do
  void (run cleanUp)
  replicateM_ 10 $ do
    runParallelCommands cmds
    void (run cleanUp)
  assert True
-- start snippet ParallelRegistry
