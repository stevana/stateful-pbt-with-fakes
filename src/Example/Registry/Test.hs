{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}

module Example.Registry.Test where

import Control.Arrow
import Control.Concurrent
import Control.Exception (ErrorCall(..), try)
import Control.Monad
import Control.Monad.State
import Data.Either
import Data.List
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Example.Registry.Real
import Parallel2
import Stateful2

------------------------------------------------------------------------

swarm :: [Gen a] -> Gen a
swarm [] = error "swarm: no generators"
swarm gens = do
  gens' <- elements (filter (not . null) (subsequences gens))
  oneof gens'

data RegState = RegState
  { tids   :: [Var ThreadId]
  , regs   :: [(String, Var ThreadId)]
  , killed :: [Var ThreadId]
  }
  deriving Show

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
    deriving (Show, Functor, Foldable, Traversable)

  data Response RegState tid
    = Spawn_ tid
    | WhereIs_ (NonFoldable (Maybe tid))
    | Register_ (Either ErrorCall ())
    | Unregister_ (Either ErrorCall ())
    | Kill_ ()
    deriving (Eq, Show, Functor, Foldable)

  generateCommand :: RegState -> Gen (Command RegState (Var ThreadId))
  generateCommand s = swarm $
    [ return Spawn ] ++
    [ Register <$> arbitraryName <*> elements (tids s) | not (null (tids s)) ] ++
    [ Unregister <$> arbitraryName
    , WhereIs <$> arbitraryName
    ] ++
    [ Kill <$> elements (tids s) | not (null (tids s)) ]

  type PreconditionFailure RegState = ()

  runFake :: Command RegState (Var ThreadId) -> RegState -> Fake RegState (Response RegState (Var ThreadId))
  runFake Spawn               s = do
    tid <- genSym
    put s { tids = tids s ++ [tid] }
    return (Spawn_ tid)
  runFake (WhereIs name)      s = return (WhereIs_ (NonFoldable (lookup name (regs s))))
  runFake (Register name tid) s
    | tid `elem` tids s
    , tid `notElem` killed s
    , name `notElem` map fst (regs s)
    , tid `notElem` map snd (regs s) = do
        put s { regs = (name, tid) : regs s }
        return (Register_ (Right ()))

    -- | tid `elem` tids s
    -- , name `elem` map fst (regs s)
    -- , tid `elem` map snd (regs s)
    -- = return (Register_ (Left (ErrorCall "bad argument")))

    | otherwise = -- preFail ()
                  return (Register_ (Left (ErrorCall "bad argument")))
  runFake (Unregister name)   s
    | name `elem` map fst (regs s) = do
        put s { regs = remove name (regs s) }
        return (Unregister_ (Right ()))
    | otherwise = -- preFail ()
                  return (Unregister_ (Left (ErrorCall "bad argument")))
    where
      remove x = filter ((/= x) . fst)
  runFake (Kill tid) s = do
    -- | tid `elem` tids s = do
        put s { killed = tid : killed s
              , regs   = remove tid (regs s)}
        return (Kill_ ())
--    | otherwise = preFail ()
    where
      remove x = filter ((/= x) . snd)

  runReal :: Command RegState ThreadId -> IO (Response RegState ThreadId)
  runReal Spawn               = Spawn_ <$> spawn
  runReal (WhereIs name)      = WhereIs_ . NonFoldable <$> whereis name
  runReal (Register name tid) = Register_ <$> fmap (left abstractError) (try (register name tid))
  runReal (Unregister name)   = Unregister_ <$> fmap (left abstractError) (try (unregister name))
  runReal (Kill tid)          = Kill_ <$> kill tid

  monitoring :: (RegState, RegState) -> Command RegState ThreadId -> Response RegState ThreadId
             -> Property -> Property
  monitoring (_s, _s') Register {} (Register_ resp) = classify (isLeft resp) (show RegisterFailed)
  monitoring (_s, s') _cmd _resp =
    counterexample $ "\n    State: " ++ show s' ++ "\n"

  runCommandMonad _ = id

-- Throws away the location information from the error, so that it matches up
-- with the fake.
abstractError :: ErrorCall -> ErrorCall
abstractError (ErrorCallWithLocation msg _loc) = ErrorCall msg

data Tag = RegisterFailed
  deriving Show

arbitraryName :: Gen String
arbitraryName = elements allNames

allNames :: [String]
allNames = ["a", "b", "c", "d", "e"]

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

prop_parallelRegistry :: ParallelCommands RegState -> Property
prop_parallelRegistry cmds = monadicIO $ do
  run resetRegistry
  replicateM_ 10 $ do
    runParallelCommands cmds
    run resetRegistry
  assert True

{-
  *** Failed! (after 42 tests and 22 shrinks):
    Exception:
      linearisable: impossible, all precondtions are satisifed during generation
      cmd = Register "d" (Var 5)
      s = RegState {tids = [Var 0,Var 1,Var 2,Var 3,Var 4], regs = [], killed = []}
      err = ()
      CallStack (from HasCallStack):
        error, called at src/Parallel2.hs:466:11 in stateful-pbt-with-fakes-0.0.0-inplace:Parallel2
    ParallelCommands {parPrefix = Commands {unCommands = [Spawn,Spawn,Spawn,Spawn,Spawn,Register "d"
(Var 5),Spawn]}, parSuffixes = [Two {proj1 = Commands {unCommands = []}, proj2 = Commands {unCommands = []}}]}
    Use --quickcheck-replay=377820 to reproduce.
-}

{-


   *** Failed! (after 1300 tests):
    Exception:
      linearisable: impossible, all precondtions are satisifed during generation
      cmd = Register "d" (Var 0)
      s = RegState {tids = [Var 0], regs = [("a",Var 0)], killed = []}
      err = ()
      CallStack (from HasCallStack):
        error, called at src/Parallel2.hs:466:11 in stateful-pbt-with-fakes-0.0.0-inplace:Parallel2
    ParallelCommands {parPrefix = Commands {unCommands = []},
  parSuffixes = [Two {proj1 = Commands {unCommands = [Unregister "c"]},
                      proj2 = Commands {unCommands = [WhereIs "b",Spawn]}}
                ,Two {proj1 = Commands {unCommands = [Register "a" (Var 0)]},
                      proj2 = Commands {unCommands = [Unregister "a",WhereIs "e"]}}
                ,Two {proj1 = Commands {unCommands = []},
                      proj2 = Commands {unCommands = [Register "d" (Var 0)]}}]}
    Use --quickcheck-replay=196456 --quickcheck-max-size=5 to reproduce.

-}

prop_shouldNotHaveBeenGenerated :: Property
prop_shouldNotHaveBeenGenerated = withMaxSuccess 1 (prop_parallelRegistry cmds)
  where
    cmds = ParallelCommands
      { parPrefix   = Commands [ Spawn ]
      , parSuffixes =
          [ Two
              [ Register "a" (Var 0) ]
              []
          , Two
              [ Register "b" (Var 0) ]
              []
          ]
      }
    -- Use --quickcheck-replay=620265 to reproduce.

prop_unregister :: Property
prop_unregister = withMaxSuccess 10 (prop_parallelRegistry cmds)
  where
    cmds = ParallelCommands
      { parPrefix   =
          [ ]
      , parSuffixes =
          [ Two
              [ Spawn ]
              [ Spawn ]
          , Two
              [ Register "a" (Var 1) ]
              [ WhereIs "a"
              ]
          ]
      }

-- --quickcheck-replay=10591
prop_parallelGeneration :: Property
prop_parallelGeneration = withMaxSuccess 10 (prop_parallelRegistry cmds)
  where
    cmds = ParallelCommands
      { parPrefix   =
          [ Spawn ]
      , parSuffixes =
          [ Two
              [ Register "c" (Var 0) ]
              [ Unregister "c" ]
          , Two
              [ Register "a" (Var 0) ] []
          ]
      }

{-
   ParallelCommands                                                                      [30/190710]
      { parPrefix   =
          [ Spawn
          , Spawn
          , Spawn
          ]
      , parSuffixes =
          [ Two []
              [ Spawn ]
          , Two []
              [ Spawn ]
          , Two
              [ Register "d" (Var 4) ] []
          , Two
              [ Kill (Var 4)
              , WhereIs "d"
              ] []
          ]
      }
    History {unHistory = [Invoke (Pid 0) Spawn,Ok (Pid 0) (Spawn_ (ThreadId 14151)),Invoke (Pid 0) Sp
awn,Ok (Pid 0) (Spawn_ (ThreadId 14152)),Invoke (Pid 0) Spawn,Ok (Pid 0) (Spawn_ (ThreadId 14153)),In
voke (Pid 2) Spawn,Ok (Pid 2) (Spawn_ (ThreadId 14156)),Invoke (Pid 2) Spawn,Ok (Pid 2) (Spawn_ (Thre
adId 14159)),Invoke (Pid 1) (Register "d" (Var 4)),Ok (Pid 1) (Register_ (Right ())),Invoke (Pid 1) (
Kill (Var 4)),Ok (Pid 1) (Kill_ ()),Invoke (Pid 1) (WhereIs "d"),Ok (Pid 1) (WhereIs_ (NonFoldable (J
ust (ThreadId 14159))))]}
    Commands length: 8
    History length: 16
    Expected: WhereIs_ (NonFoldable Nothing)
    Got: WhereIs_ (NonFoldable (Just (ThreadId 14159)))
    Use --quickcheck-replay=491978 to reproduce.

-}

prop_race :: Property
prop_race = prop_parallelRegistry cmds
  where
    cmds = ParallelCommands
      { parPrefix = [ Spawn, Kill (Var 0) ]
      , parSuffixes =
          [ Two
              [ Register "a" (Var 0) ]
              [ Register "a" (Var 0) ]
          ]
      }
