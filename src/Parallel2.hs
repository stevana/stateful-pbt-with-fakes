{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module Parallel2 where

-- import Debug.Trace
import Control.Arrow
import Control.Monad
-- import qualified Text.PrettyPrint.Annotated.HughesPJ as PP
import Control.Concurrent
import Control.Concurrent.Async
import qualified Data.IntMap.Strict as IntMap
import Control.Concurrent.STM
import Control.Exception (SomeException, try, displayException, evaluate)
import Control.Monad.IO.Class
import Data.Foldable
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Proxy
import Data.Tree
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Stateful2
import AtomicCounter

------------------------------------------------------------------------

data ParallelCommands state = ParallelCommands
  { parPrefix   :: Commands state
  , parSuffixes :: [Fork (Commands state)]
  }
deriving stock instance
  Eq (Commands state) => Eq (ParallelCommands state)

instance Show (Command state (Var (Reference state))) => Show (ParallelCommands state) where
  showsPrec _ (ParallelCommands prefix suffixes)
    = showString "ParallelCommands"
    . showString "\n  { parPrefix   = "
    . showCommands 0 prefix
    . showString "\n  , parSuffixes = "
    . showSuffixes suffixes
    . showString "\n  }"
    where
      showCommands _n (Commands [])    = showString "[]"
      showCommands  n (Commands [cmd])
        = showString "\n" . showString (replicate n ' ' ++ "      [ ")
        . shows cmd . showString " ]"
      showCommands  n (Commands cmds0)
        = showString "\n" . showString (replicate n ' ' ++ "      [ ")
        . go cmds0 . showString "\n" . showString (replicate n ' ' ++ "      ]")
        where
          go []           = id
          go [cmd]        = shows cmd
          go (cmd : cmds) = shows cmd . showString "\n" . showString (replicate n ' ' ++ "      , ") . go cmds
      showSuffixes []     = showString "[]"
      showSuffixes forks0 = showString "\n      [ " . go forks0 . showString "\n      ]"
        where
          go [] = id
          go [Two l r]
            = showString "Two "
            . showCommands 4 l
            . showString " "
            . showCommands 4 r
          go (Two l r : forks)
            = showString "Two "
            . showCommands 4 l
            . showString " "
            . showCommands 4 r
            . showString "\n      , "
            . go forks

lengthParallelCommands :: ParallelCommands state -> Int
lengthParallelCommands (ParallelCommands prefix suffixes) =
  length (unCommands prefix) +
  go suffixes
  where
    go [] = 0
    go (Two l r : rest) = length (unCommands l) + length (unCommands r) + go rest

data Fork a = Two
  { proj1 :: !a
  , proj2 :: !a
  }
  deriving stock (Eq, Show, Functor, Foldable)

toFork :: (a, a) -> Fork a
toFork (l, r) = Two l r

fromFork :: Fork a -> (a, a)
fromFork (Two l r) = (l, r)

------------------------------------------------------------------------

instance StateModel state => Arbitrary (ParallelCommands state) where

  arbitrary :: Gen (ParallelCommands state)
  arbitrary = do
    Commands cmds <- arbitrary
    prefixLength <- sized (\k -> choose (0, k `div` 3))
    let (prefix_, rest) = splitAt prefixLength cmds
        prefix = Commands prefix_
        suffixes = uncurry makeSuffixes (nextStateParallel initialState initialGenSym prefix) rest
    return (ParallelCommands prefix suffixes)
    where
      makeSuffixes :: state -> GenSym -> [Command state (Var (Reference state))]
                   -> [Fork (Commands state)]
      makeSuffixes _s _gsym []   = []
      makeSuffixes  s  gsym cmds =
        Two (Commands safe1) (Commands safe2) :
          uncurry makeSuffixes (nextStateParallel s gsym (Commands safe)) rest
        where
          (safe, rest)   = safeSpan s gsym cmds
          (safe1, safe2) = splitAt (length safe `div` 2) safe

  shrink :: ParallelCommands state -> [ParallelCommands state]
  shrink (ParallelCommands prefix suffixes)
    = concatMap go
       [ Shrunk s (ParallelCommands prefix' (map (uncurry Two) suffixes'))
       | Shrunk s (prefix', suffixes') <- shrinkPairS shrinkCommands' shrinkSuffixes
                                                      (prefix, map fromFork suffixes)
       ]
       ++
       shrinkMoveSuffixToPrefix
   where
     go :: Shrunk (ParallelCommands state) -> [ParallelCommands state]
     go (Shrunk shrunk0 cmds) =
         shrinkAndValidateParallel (if shrunk0 then DontShrink else MustShrink)
                                   cmds

     shrinkSuffixes :: [(Commands state, Commands state)]
                    -> [Shrunk [(Commands state, Commands state)]]
     shrinkSuffixes = shrinkListS (shrinkPairS' shrinkCommands')

     -- Moving a command from a suffix to the prefix preserves validity
     shrinkMoveSuffixToPrefix :: [ParallelCommands state]
     shrinkMoveSuffixToPrefix = case suffixes of
       []                   -> []
       (suffix : suffixes') ->
         [ ParallelCommands (prefix <> Commands [prefix'])
                            (fmap Commands (toFork suffix') : suffixes')
         | (prefix', suffix') <- pickOneReturnRest2 (unCommands (proj1 suffix),
                                                     unCommands (proj2 suffix))
         ]

data ShouldShrink = MustShrink | DontShrink

data Shrunk a = Shrunk { wasShrunk :: Bool, shrunk :: a }
  deriving Functor

shrinkS :: Arbitrary a => a -> [Shrunk a]
shrinkS a = map (Shrunk True) (shrink a) ++ [Shrunk False a]

shrinkListS :: forall a. (a -> [Shrunk a]) -> [a] -> [Shrunk [a]]
shrinkListS f = \xs -> concat [
      map (Shrunk True) (shrinkList (const []) xs)
    , shrinkOne xs
    , [Shrunk False xs]
    ]
  where
    shrinkOne :: [a] -> [Shrunk [a]]
    shrinkOne []     = []
    shrinkOne (x:xs) = [Shrunk True (x' : xs) | Shrunk True x'  <- f x]
                    ++ [Shrunk True (x : xs') | Shrunk True xs' <- shrinkOne xs]

-- | Shrink list without shrinking elements.
shrinkListS' :: [a] -> [Shrunk [a]]
shrinkListS' = shrinkListS (\a -> [Shrunk False a])

-- | Shrink list by only shrinking elements.
shrinkListS'' :: forall a. (a -> [Shrunk a]) -> [a] -> [Shrunk [a]]
shrinkListS'' f xs =
  let shr = shrinkListS f xs
      len = length xs
  in filter (\s -> length (shrunk s) == len) shr

shrinkPairS :: (a -> [Shrunk a])
            -> (b -> [Shrunk b])
            -> (a, b) -> [Shrunk (a, b)]
shrinkPairS f g (a, b) =
       [Shrunk True (a', b) | Shrunk True a' <- f a ]
    ++ [Shrunk True (a, b') | Shrunk True b' <- g b ]
    ++ [Shrunk False (a, b)]

shrinkPairS' :: (a -> [Shrunk a]) -> (a, a) -> [Shrunk (a, a)]
shrinkPairS' f = shrinkPairS f f


-- | Shrink commands in a pre-condition and scope respecting way.
shrinkCommands ::  forall state. StateModel state
               => Commands state -> [Commands state]
shrinkCommands =
    concatMap go . shrinkListS' . unCommands
  where
    go :: Shrunk [Command state (Var (Reference state))] -> [Commands state]
    go (Shrunk shrunk0 cmds) = map snd $
        shrinkAndValidate (if shrunk0 then DontShrink else MustShrink)
                          (initValidateEnv initialState)
                          (Commands cmds)

-- >    pickOneReturnRest2 ([], []) == []
-- >    pickOneReturnRest2 ([1,2], [3,4])
-- > == [ (1,([2],[3,4])), (2,([1],[3,4])), (3,([1,2],[4])), (4,([1,2],[3])) ]
pickOneReturnRest2 :: ([a], [a]) -> [(a, ([a],[a]))]
pickOneReturnRest2 (xs, ys) =
  map (second (,ys)) (pickOneReturnRest xs) ++
  map (second (xs,)) (pickOneReturnRest ys)

-- >    pickOneReturnRest []     == []
-- >    pickOneReturnRest [1]    == [ (1,[]) ]
-- >    pickOneReturnRest [1..3] == [ (1,[2,3]), (2,[1,3]), (3,[1,2]) ]
pickOneReturnRest :: [a] -> [(a, [a])]
pickOneReturnRest []       = []
pickOneReturnRest (x : xs) = (x, xs) : map (second (x :)) (pickOneReturnRest xs)

newtype Counter = Counter Int

newCounter :: Counter
newCounter = Counter 0

-- | Environment required during 'shrinkAndValidate'
data ValidateEnv state = ValidateEnv {
      -- | The model we're starting validation from
      veModel   :: state

      -- | Reference renumbering
      --
      -- When a command
      --
      -- > Command .. [Var i, ..]
      --
      -- is changed during validation to
      --
      -- > Command .. [Var j, ..]
      --
      -- then any subsequent uses of @Var i@ should be replaced by @Var j@. This
      -- is recorded in 'veScope'. When we /remove/ the first command
      -- altogether (during shrinking), then @Var i@ won't appear in the
      -- 'veScope' and shrank candidates that contain commands referring to @Var
      -- i@ should be considered as invalid.
    , veScope   :: Map (Var (Reference state)) (Var (Reference state))

      -- | Counter (for generating new references)
    , veCounter :: GenSym
    }

initValidateEnv :: state -> ValidateEnv state
initValidateEnv initState = ValidateEnv
  { veModel   = initState
  , veScope   = M.empty
  , veCounter = initialGenSym
  }

combineEnv :: StateModel state
           => ValidateEnv state
           -> ValidateEnv state
           -> Commands state
           -> ValidateEnv state
combineEnv envL envR cmds = ValidateEnv
  { veModel   = fst (foldl' (uncurry nextState) (veModel envL, veCounter envL) (unCommands cmds))
  , veScope   = M.union (veScope envL) (veScope envR)
  , veCounter = veCounter envR
  }

withCounterFrom :: ValidateEnv state -> ValidateEnv state -> ValidateEnv state
withCounterFrom e e' = e { veCounter = veCounter e' }

-- | Validate list of commands, optionally shrinking one of the commands
--
-- The input to this function is a list of commands ('Commands'), for example
--
-- > [A, B, C, D, E, F, G, H]
--
-- The /result/ is a /list/ of 'Commands', i.e. a list of lists. The
-- outermost list is used for all the shrinking possibilities. For example,
-- let's assume we haven't shrunk something yet, and therefore need to shrink
-- one of the commands. Let's further assume that only commands B and E can be
-- shrunk, to B1, B2 and E1, E2, E3 respectively. Then the result will look
-- something like
--
-- > [    -- outermost list recording all the shrink possibilities
-- >     [A', B1', C', D', E' , F', G', H']   -- B shrunk to B1
-- >   , [A', B2', C', D', E' , F', G', H']   -- B shrunk to B2
-- >   , [A', B' , C', D', E1', F', G', H']   -- E shrunk to E1
-- >   , [A', B' , C', D', E2', F', G', H']   -- E shrunk to E2
-- >   , [A', B' , C', D', E3', F', G', H']   -- E shrunk to E3
-- > ]
--
-- where one of the commands has been shrunk and all commands have been
-- validated and renumbered (references updated). So, in this example, the
-- result will contain at most 5 lists; it may contain fewer, since some of
-- these lists may not be valid.
--
-- If we _did_ already shrink something, then no commands will be shrunk, and
-- the resulting list will either be empty (if the list of commands was invalid)
-- or contain a /single/ element with the validated and renumbered commands.
shrinkAndValidate :: forall state. StateModel state
                  => ShouldShrink
                  -> ValidateEnv state
                  -> Commands state
                  -> [(ValidateEnv state, Commands state)]
shrinkAndValidate shouldShrink0 state0 cmds0 =
  map (second Commands) $ go shouldShrink0 state0 (unCommands cmds0)
  where
    go :: ShouldShrink -> ValidateEnv state -> [Command state (Var (Reference state))]
       -> [(ValidateEnv state, [Command state (Var (Reference state))])]
    go MustShrink   _   [] = []          -- we failed to shrink anything
    go DontShrink   env [] = [(env, [])] -- successful termination
    go shouldShrink (ValidateEnv state scope gsym) (cmd' : cmds) =
      case traverse (remapVars scope) cmd' of
        Just remapped ->
          -- shrink at most one command
          let candidates :: [(ShouldShrink, Command state (Var (Reference state)))]
              candidates =
                case shouldShrink of
                  DontShrink -> [(DontShrink, remapped)]
                  MustShrink -> map (DontShrink,) (shrinkCommand state remapped)
                             ++ [(MustShrink, remapped)]
          in flip concatMap candidates $ \(shouldShrink', cmd) ->
               case (runRunFake (runFake cmd' state) state gsym, runRunFake (runFake cmd state) state gsym) of
                 (Left _, Left _err) -> []
                 (Right ((resp', _), _), Right ((resp, state'), gsym')) ->
                   let vars' = toList resp'
                       vars  = toList resp
                       env'  = ValidateEnv
                                 { veModel   = state'
                                 , veScope   = M.fromList (zip vars' vars) `M.union` scope
                                 , veCounter = gsym'
                                 }
                   in map (second (cmd :)) $ go shouldShrink' env' cmds
                 (Left _, Right _) -> error "shrinkAndValidate: impossible."
                 (Right _, Left _) -> error "shrinkAndValidate: impossible."
        Nothing ->
          []

    remapVars :: Map (Var a) (Var a) -> Var a -> Maybe (Var a)
    remapVars scope v = M.lookup v scope


-- | Shrinks Commands in a way that it has strictly less number of commands.
shrinkCommands' :: Commands state -> [Shrunk (Commands state)]
shrinkCommands' = map (fmap Commands) . shrinkListS' . unCommands

shrinkAndValidateParallel :: forall state. StateModel state
                          => ShouldShrink
                          -> ParallelCommands state
                          -> [ParallelCommands state]
shrinkAndValidateParallel shouldShrink0 (ParallelCommands prefix suffixes0) =
  let
    env = initValidateEnv initialState
    curryGo shouldShrink' (env', prefix') = go prefix' env' shouldShrink' suffixes0
  in
    case shouldShrink0 of
      DontShrink -> concatMap (curryGo DontShrink) (shrinkAndValidate DontShrink env prefix)
      MustShrink -> concatMap (curryGo DontShrink) (shrinkAndValidate MustShrink env prefix)
                 ++ concatMap (curryGo MustShrink) (shrinkAndValidate DontShrink env prefix)
  where
    go :: Commands state             -- validated prefix
       -> ValidateEnv state          -- environment after the prefix
       -> ShouldShrink               -- should we /still/ shrink something?
       -> [Fork (Commands state)]    -- suffixes to validate
       -> [ParallelCommands state]
    go prefix' = go' []
      where
        go' :: [Fork (Commands state)] -- accumulated validated suffixes (in reverse order)
            -> ValidateEnv state       -- environment after the validated suffixes
            -> ShouldShrink            -- should we /still/ shrink something?
            -> [Fork (Commands state)] -- suffixes to validate
            -> [ParallelCommands state]
        go' _   _   MustShrink [] = [] -- Failed to shrink something
        go' acc _   DontShrink [] = [ParallelCommands prefix' (reverse acc)]
        go' acc env shouldShrink (Two l r : suffixes) = do
            ((shrinkL, shrinkR), shrinkRest) <- shrinkOpts
            (envL, l') <- shrinkAndValidate shrinkL  env                         l
            (envR, r') <- shrinkAndValidate shrinkR (env `withCounterFrom` envL) r
            go' (Two l' r' : acc) (combineEnv envL envR r') shrinkRest suffixes
          where
            shrinkOpts :: [((ShouldShrink, ShouldShrink), ShouldShrink)]
            shrinkOpts =
                case shouldShrink of
                  DontShrink -> [ ((DontShrink, DontShrink), DontShrink) ]
                  MustShrink -> [ ((MustShrink, DontShrink), DontShrink)
                                , ((DontShrink, MustShrink), DontShrink)
                                , ((DontShrink, DontShrink), MustShrink) ]



{-
withParStates :: StateModel state
              => [[Command state (Var (Reference state))]]
              -> [[(Command state (Var (Reference state)), state)]]
withParStates = go initialState
  where
    go _s []             = []
    go  s (cmds : cmdss) =
      let
        (s', cmdsAndStates) = withStates s cmds
      in
        cmdsAndStates : go s' cmdss
-}


safeSpan :: StateModel state
         => state -> GenSym -> [Command state (Var (Reference state))]
         -> ([Command state (Var (Reference state))],
             [Command state (Var (Reference state))])
safeSpan s gsym = go []
  where
    go safe [] = (reverse safe, [])
    go safe (cmd : cmds)
      | length safe <= 5
      , parSafe s gsym (cmd : safe) = go (cmd : safe) cmds
      | otherwise                   = (reverse safe, cmd : cmds)

parSafe :: StateModel state
        => state -> GenSym -> [Command state (Var (Reference state))] -> Bool
parSafe s0 gsym0 = all (validCommands s0 gsym0) . permutations
  where
    validCommands :: StateModel state
                  => state -> GenSym -> [Command state (Var (Reference state))] -> Bool
    validCommands _s _gsym []           = True
    validCommands  s  gsym (cmd : cmds)
      | precondition s gsym cmd = uncurry validCommands (nextState s gsym cmd) cmds
      | otherwise               = False

nextStateParallel :: StateModel state => state -> GenSym -> Commands state -> (state, GenSym)
nextStateParallel s gsym = foldl' (uncurry nextState) (s, gsym) . unCommands

  {-
validParallelCommands :: StateModel state => ParallelCommands state -> Bool
validParallelCommands (ParallelCommands prefix suffixes) =
  let
    s = foldr nextState initialStat
  -- ego initialState cmdss0
  where
    go _s [] = True
    go s (cmds : cmdss) | parSafe s cmds = go (nextStateParallel s cmds) cmdss
                        | otherwise      = False
-}

------------------------------------------------------------------------

newtype History state = History { unHistory :: [Event state] }
deriving stock instance
   (Show (Command state (Var (Reference state))),
    Show (Response state (Reference state))) => Show (History state)

data Event state
  = Invoke Pid (Command state (Var (Reference state)))
  | Ok     Pid (Response state (Reference state))
deriving stock instance
  (Show (Command state (Var (Reference state))),
   Show (Response state (Reference state))) => Show (Event state)

newtype Pid = Pid Int
  deriving stock (Eq, Ord, Show)

toPid :: ThreadId -> Pid
toPid tid = Pid (read (drop (length ("ThreadId " :: String)) (show tid)))

data Op state = Op (Command state (Var (Reference state)))
                   (Response state (Reference state))

------------------------------------------------------------------------

interleavings :: History state -> Forest (Op state)
interleavings (History [])  = []
interleavings (History evs0) =
  [ Node (Op cmd resp) (interleavings (History evs'))
  | (tid, cmd)   <- takeInvocations evs0
  , (resp, evs') <- findResponse tid
                      (filter1 (not . matchInvocation tid) evs0)
  ]
  where
    takeInvocations :: [Event state] -> [(Pid, Command state (Var (Reference state)))]
    takeInvocations []                         = []
    takeInvocations ((Invoke pid cmd)   : evs) = (pid, cmd) : takeInvocations evs
    takeInvocations ((Ok    _pid _resp) : _)   = []

    findResponse :: Pid -> [Event state] -> [(Response state (Reference state), [Event state])]
    findResponse _pid []                                   = []
    findResponse  pid ((Ok pid' resp) : evs) | pid == pid' = [(resp, evs)]
    findResponse  pid (ev             : evs)               =
      [ (resp, ev : evs') | (resp, evs') <- findResponse pid evs ]

    matchInvocation :: Pid -> Event state -> Bool
    matchInvocation pid (Invoke pid' _cmd) = pid == pid'
    matchInvocation _   _                  = False

    filter1 :: (a -> Bool) -> [a] -> [a]
    filter1 _ []                   = []
    filter1 p (x : xs) | p x       = x : filter1 p xs
                       | otherwise = xs

  {-
linearisable :: forall state. StateModel state
             => [(Int, Reference state)] -> Forest (Op state) -> Bool
linearisable _env = any' (go initialState initialGenSym emptyEnv)
  where
    go :: state -> GenSym -> Env state -> Tree (Op state) -> Bool
    go s gsym env (Node (Op cmd cresp) ts) =
      case runRunFake (runFake cmd s) s gsym of
        Left err ->
          error $ "linearisable: impossible, all preconditions are satisifed during generation\ncmd = " ++
                  show cmd ++ "\ns = " ++ show s ++ "\nerr = " ++ show err
        Right ((resp, s'), gsym') ->
          let env' = extendEnv_ env (toList cresp) in
          -- traceShow (s, env, cmd, resp, env', s') $
          cresp == fmap (lookupEnv env') resp &&
          any' (go s' gsym' env') ts

    any' :: (a -> Bool) -> [a] -> Bool
    any' _p [] = True
    any'  p xs = any p xs
-}

linearisableCE :: forall state. StateModel state
               => Forest (Op state) -> Either String ()
linearisableCE = any' (go initialState initialGenSym emptyEnv)
  where
    go :: state -> GenSym -> Env state -> Tree (Op state) -> Either String ()
    go !s !gsym !env (Node (Op !cmd !cresp) ts) =
      case runRunFake (runFake cmd s) s gsym of
        Left err -> Left ("Precondition failed: " ++ show err)
          -- error $ "linearisable: impossible, all preconditions are satisifed during generation\ncmd = " ++
          --         show cmd ++ "\ns = " ++ show s ++ "\nerr = " ++ show err
        Right ((!resp, !s'), !gsym') ->
          let
            !env'   = extendEnv_ env (toList cresp)
            !cresp' = fmap (lookupEnv env') resp
          in
            -- traceShow (s, env, cmd, resp, env', s') $
            if cresp == cresp'
            then any' (go s' gsym' env') ts
            else Left ("Expected: " ++ show cresp' ++ "\nGot: " ++ show cresp)

any' :: (a -> Either String ()) -> [a] -> Either String ()
any' _p [] = Right ()
any'  p xs = anyE "impossible" p xs

anyE :: String -> (a -> Either String ()) -> [a] -> Either String ()
anyE  e _p [] = Left e
anyE _e  p (x : xs) = case p x of
  Left e -> anyE e p xs
  Right () -> Right ()
  {-

linearisableP :: forall state. StateModel state
              => Forest (Op state) -> Property
linearisableP = anyP' (go initialState initialGenSym [])
  where
    go :: state -> GenSym -> [(Int, Reference state)] -> Tree (Op state) -> Property
    go s gsym env (Node (Op cmd cresp) ts) =
      case runRunFake (runFake cmd s) s gsym of
        Left err ->
          error $ "linearisable: impossible, all preconditions are satisifed during generation\ncmd = " ++
                  show cmd ++ "\ns = " ++ show s ++ "\nerr = " ++ show err
        Right ((resp, s'), gsym') ->
          let env' = env ++ zip [length env..] (toList cresp) in
          -- traceShow (s, env, cmd, resp, env', s') $
          -- counterexample (show cmd ++ " -->" ++ show cresp) (
          cresp === fmap (lookupEnv env') resp .&&.
          anyP' (go s' gsym' env') ts

    anyP' :: (a -> Property) -> [a] -> Property
    anyP' _p [] = property True
    anyP'  p xs = anyP p xs

    anyP :: (a -> Property) -> [a] -> Property
    anyP p = foldl' (\ih x -> p x .||. ih) (property False)

-- | Lifts a plain property into a monadic property.
liftProperty :: Monad m => Property -> PropertyM m ()
liftProperty prop = MkPropertyM (\k -> fmap (prop .&&.) <$> k ())
  -}

------------------------------------------------------------------------

runParallelCommands :: forall state. StateModel state
                    => ParallelCommands state -> PropertyM (CommandMonad state) ()
runParallelCommands cmds@(ParallelCommands prefix suffixes) = do
  forM_ (unCommands prefix ++ concatMap (concatMap unCommands . toList) suffixes) $ \cmd ->
    let name = commandName cmd in
      monitor (tabulate "Commands" [name] . classify True name)
  let len = lengthParallelCommands cmds
  monitor ( classify (len == 0) "Total commands: 0"
          . classify (len >  0  && len <= 10)  "Total commands: 1-10"
          . classify (len > 10  && len <= 20)  "Total commands: 11-20"
          . classify (len > 20  && len <= 30)  "Total commands: 21-30"
          . classify (len > 30  && len <= 40)  "Total commands: 31-40"
          . classify (len > 40  && len <= 50)  "Total commands: 41-50"
          . classify (len > 50  && len <= 60)  "Total commands: 51-60"
          . classify (len > 60  && len <= 100) "Total commands: 61-100"
          . classify (len > 100 && len <= 200) "Total commands: 101-200"
          . classify (len > 200) "Total commands: >200")
  monitor (tabulate "Number of concurrent commands" (map (show . length) suffixes))
  evs <- liftIO newTQueueIO :: PropertyM (CommandMonad state) (TQueue (Event state))
  c   <- liftIO newAtomicCounter
  env <- liftIO (runCommandsHistory (Pid 0) evs c emptyEnv prefix)
  runSuffixes evs c env suffixes
  hist <- History <$> liftIO (atomically (flushTQueue evs))
  -- monitor (counterexample (prettyHistory hist))
  -- liftProperty (linearisableP (interleavings hist))

  -- let ok = linearisable env' (interleavings hist)
  -- unless ok $
  --    monitor (counterexample (prettyHistory hist))
  -- assert ok
  case linearisableCE (interleavings hist) of
    Left ce -> do
      monitor (counterexample (prettyHistory hist))
      monitor (counterexample ("Commands length: " ++ show (lengthParallelCommands cmds)))
      monitor (counterexample ("History length: " ++ show (length (unHistory hist))))
      monitor (counterexample ce)
      assert False
    Right () -> assert True
  where
    runSuffixes :: TQueue (Event state) -> AtomicCounter -> Env state
                -> [Fork (Commands state)]
                -> PropertyM (CommandMonad state) ()
    runSuffixes evs c = go
      where
        go _env [] = return ()
        go  env (Two cmdsL cmdsR : cmdss) = do
          (envL, envR) <- liftIO $
            concurrently
              (runCommandsHistory (Pid 1) evs c env cmdsL)
              (runCommandsHistory (Pid 2) evs c env cmdsR)
          let env' = envL <> envR
          go env' cmdss

runCommandsHistory :: forall state. StateModel state
                   => Pid -> TQueue (Event state) -> AtomicCounter
                   -> Env state
                   -> Commands state -> IO (Env state)
runCommandsHistory pid evs c env0 = go env0 . unCommands
  where
    go :: Env state -> [Command state (Var (Reference state))] -> IO (Env state)
    go env []           = return env
    go env (cmd : cmds) = do
      atomically (writeTQueue evs (Invoke pid cmd))
      case subst env cmd of
        Nothing ->
      eResp <- try (runCommandMonad (Proxy :: Proxy state) (runReal (fmap (lookupEnv env) cmd)))
      eResp' <- evaluate eResp
      case eResp' of
        Left (err :: SomeException) ->
          error ("runParallelCommands: " ++ displayException err)
        Right !resp -> do
          let refs = toList resp
          env' <- extendEnv env c refs
          atomically (writeTQueue evs (Ok pid resp))
          go env' cmds

extendEnv :: Env state -> AtomicCounter -> [Reference state] -> IO (Env state)
extendEnv env c refs = do
  i <- incrAtomicCounter c (length refs)
  return (env <> Env (IntMap.fromList (zip [i..] refs)))

------------------------------------------------------------------------

prettyHistory :: forall state. StateModel state => History state -> String
prettyHistory = show

  {-
showString "\nHistory:\n" . flip go "" . unHistory
  where
    go :: [Event state] -> String -> String
    go [] = id
    go (Invoke (Pid pid) cmd : Ok (Pid pid') resp : evs)
      | pid == pid' = showString "  [" . shows pid . showString "] "
                    . shows cmd
                    . showString " --> "
                    . shows resp
                    . showString "\n" . go evs
                    . go evs
      -- | pid == pid'
      -- , resp /= resp' = showString "  [" . shows pid . showString "] "
                      -- . shows cmd
                      -- . showString " -/> "
                      -- . shows resp
                      -- . showString "(expected: "
                      -- . shows resp'
                      -- . showString ")"
                      -- . showString "\n" . go s gsym evs
                      -- . go s' gsym' evs
      | otherwise = showString "  [" . shows pid . showString "] "
                  . shows cmd
                  . showString " --> ...\n"
                  . go (Ok (Pid pid') resp : evs)
    go  (Invoke (Pid pid) cmd : invoke@(Invoke _ _) : evs)
      = showString "  [" . shows pid . showString "] "
      . shows cmd
      . showString " --> ...\n"
      . go (invoke : evs)
    go  (Ok (Pid pid) resp : evs)
      = showString "  [" . shows pid . showString "] ... "
      . shows resp . showString "\n" . go evs
    go (Invoke {} : []) = error "prettyHistory: impossible"
-}

------------------------------------------------------------------------

{-

-- | Draw an ASCII diagram of the history of a parallel program. Useful for
--   seeing how a race condition might have occured.
toBoxDrawings :: forall cmd resp ann. Rank2.Foldable cmd
              => (Show (cmd Concrete), Show (resp Concrete))
              => ParallelCommands cmd resp -> History cmd resp -> Doc ann
toBoxDrawings (ParallelCommands prefix suffixes) = toBoxDrawings'' allVars
  where
    allVars = getAllUsedVars prefix `S.union`
                foldMap (foldMap getAllUsedVars) suffixes

    toBoxDrawings'' :: Set Var -> History cmd resp -> Doc ann
    toBoxDrawings'' knownVars (History h) = mconcat
        ([ exec evT (fmap (out  . snd) <$> Fork l p r)
         , PP.line
         , PP.line
         ]
         ++ map ppException exceptions
        )
      where
        (_, exceptions, h'') = foldl'
               (\(i, excs, evs) (pid, ev) ->
                  case ev of
                    Exception err -> (i + 1, excs ++ [(i, err)], evs ++ [(pid, Exception $ " Exception " <> show i)])
                    _ -> (i, excs, evs ++ [(pid, ev)])
               )
               (0 :: Int, [], [])
               h
        (p, h') = partition (\e -> fst e == Pid 0) h''
        (l, r)  = partition (\e -> fst e == Pid 1) h'

        out :: HistoryEvent cmd resp -> String
        out (Invocation cmd vars)
          | vars `S.isSubsetOf` knownVars = show (S.toList vars) ++ " ← " ++ show cmd
          | otherwise                     = show cmd
        out (Response resp) = show resp
        out (Exception err) = err

        toEventType :: History' cmd resp -> [(EventType, Pid)]
        toEventType = map go
          where
            go e = case e of
              (pid, Invocation _ _) -> (Open,  pid)
              (pid, Response   _)   -> (Close, pid)
              (pid, Exception  _)   -> (Close, pid)

        evT :: [(EventType, Pid)]
        evT = toEventType (filter (\e -> fst e `Prelude.elem` map Pid [1, 2]) h)

        ppException :: (Int, String) -> Doc ann
        ppException (idx, err) = mconcat
         [ PP.pretty $ "Exception " <> show idx <> ":"
         , PP.line
         , PP.indent 2 $ PP.pretty err
         , PP.line
         , PP.line
         ]

-}
