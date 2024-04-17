{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}

module Registry.Test where

import Control.Concurrent
import Control.Exception (ErrorCall(ErrorCall), fromException, try)
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Registry.Real
import Stateful

------------------------------------------------------------------------

data RegState = RegState {
    tids :: [Var (ThreadId)],
    regs :: [(String, Var (ThreadId))]
  }
  deriving Show

instance StateModel RegState where

  initialState :: RegState
  initialState = RegState [] []

  type Reference RegState = ThreadId

  data Command RegState tid where
    Spawn      :: Command RegState tid
    WhereIs    :: String -> Command RegState tid
    Register   :: String -> tid -> Command RegState tid
    Unregister :: String -> Command RegState tid
    deriving (Show, Functor)

  data Response RegState tid
    = Spawn' tid
    | WhereIs' (Maybe tid)
    | Register' ()
    | Unregister' ()
    deriving (Eq, Show, Functor, Foldable)

  generateCommand :: RegState -> Gen (Symbolic RegState)
  generateCommand s = oneof
    [ return Spawn
    , Register <$> arbitraryName <*> elements (tids s)
    , Unregister <$> arbitraryName
    , WhereIs <$> arbitraryName
    ]

  type Failure RegState = ()

  runFake :: Symbolic RegState -> RegState -> Return () (RegState, Response RegState (Var (Reference RegState)))
  runFake Spawn               s = let tid = Var (length (tids s)) in
                                  Ok (s { tids = tids s ++ [tid] }, Spawn' tid)
  runFake (WhereIs name)      s = Ok (s, WhereIs' (lookup name (regs s)))
  runFake (Register name tid) s
    | tid `elem` tids s && name `notElem` map fst (regs s) && tid `notElem` map snd (regs s) =
       return (s { regs = (name, tid) : regs s }, Register' ())

    | tid `elem` tids s && tid `elem` map snd (regs s) = Throw ()
    | otherwise =
       Precondition ()
  runFake (Unregister name)   s
    | name `elem` map fst (regs s) =
        return (s { regs = remove name (regs s) }, Unregister' ())
    | otherwise =
        Precondition ()
    where
      remove x = filter ((/= x) . fst)

  runReal :: Concrete RegState -> IO (Response RegState (Reference RegState))
  runReal Spawn               = Spawn'      <$> forkIO (threadDelay 10000000)
  runReal (WhereIs name)      = WhereIs'    <$> whereis name
  runReal (Register name tid) = Register'   <$> register name tid
  runReal (Unregister name)   = Unregister' <$> unregister name

  abstractFailure _state err = case fromException err of
    Just (ErrorCall "bad argument") -> Just ()
    _otherwise                      -> Nothing

  monitoring :: (RegState, RegState) -> Concrete RegState -> Response RegState (Reference RegState)
             -> Property -> Property
  monitoring (_s, s') _cmd _resp =
    counterexample $ "\n    State: "++show s'++"\n"

  runCommandMonad _ = id

arbitraryName :: Gen String
arbitraryName = elements allNames

allNames :: [String]
allNames = ["a", "b", "c", "d", "e"]

prop_Registry :: Commands RegState -> Property
prop_Registry cmds = monadicIO $ do
  _ <- run cleanUp
  _ <- runCommands cmds
  assert True

cleanUp :: IO [Either ErrorCall ()]
cleanUp = sequence
  [ try (unregister name) :: IO (Either ErrorCall ())
  | name <- allNames
  ]

kill :: ThreadId -> IO ()
kill tid = do
  killThread tid
  yield
