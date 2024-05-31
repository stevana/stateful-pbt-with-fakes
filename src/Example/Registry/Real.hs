-- This code is mostly taken from John Hughes' Midlands Graduate School 2019
-- course:
--
--   https://www.cse.chalmers.se/~rjmh/MGS2019/


-- A simple local name service for threads... behaves like the Erlang
-- process registry.

-- If you are doing the associated QuickCheck exercises, DO NOT READ
-- THIS CODE!!! The exercise is one in black box testing.


module Example.Registry.Real where

import Data.IORef
import Control.Monad
import Control.Concurrent
import System.IO.Unsafe
import GHC.Conc (threadStatus, ThreadStatus(..))

alive :: ThreadId -> IO Bool
alive tid = do
  s <- threadStatus tid
  return $ s /= ThreadFinished && s /= ThreadDied

{-# NOINLINE registry #-}
registry :: IORef [(String,ThreadId)]
registry = unsafePerformIO (newIORef [])

{-# NOINLINE lock #-}
lock :: MVar ()
lock = unsafePerformIO (newMVar ())

whereis :: String -> IO (Maybe ThreadId)
whereis name = do
  reg <- readRegistry
  return $ lookup name reg

register :: String -> ThreadId -> IO ()
register name tid =
  -- XXX: withMVar lock $ \_ ->
  registerRace name tid
  -- registerBroken name tid

registerRace :: String -> ThreadId -> IO ()
registerRace name tid = do
  ok <- alive tid
  threadDelay 1000
  reg <- readRegistry
  threadDelay 1000
  if ok && name `notElem` map fst reg && tid `notElem` map snd reg
    then do
      threadDelay 1000
      atomicModifyIORef' registry $ \reg' ->
           if name `notElem` map fst reg' && tid `notElem` map snd reg'
             then ((name,tid):reg',())
             else (reg',badarg)
    else badarg

registerBroken :: String -> ThreadId -> IO ()
registerBroken name tid = do
  ok <- alive tid
  reg <- readRegistry
  if ok && name `notElem` map fst reg && tid `notElem` map snd reg
    then atomicModifyIORef' registry $ \reg' ->
           if name `notElem` map fst reg' && tid `notElem` map snd reg'
             then ([(name,tid)],())
             else (reg',badarg)
    else badarg

unregister :: String -> IO ()
unregister name = do
  reg <- readRegistry
  threadDelay 1000
  if name `elem` map fst reg
    then atomicModifyIORef' registry $ \reg' ->
           (filter ((/=name).fst) reg',
            ())
    else badarg

readRegistry :: IO [(String, ThreadId)]
readRegistry = do
  reg <- readIORef registry
  garbage <- filterM (fmap not.alive) (map snd reg)
  threadDelay 1000
  atomicModifyIORef' registry $ \reg' ->
    let reg'' = filter ((`notElem` garbage).snd) reg' in (reg'',reg'')

badarg :: a
badarg = error "bad argument"


resetRegistry :: IO ()
resetRegistry = atomicWriteIORef registry []

spawn :: IO ThreadId
spawn = forkIO (threadDelay 100000000)

kill :: ThreadId -> IO ()
kill tid = do
  killThread tid
  waitUntilDead 1000
  where
    waitUntilDead :: Int -> IO ()
    waitUntilDead 0 = error "kill: thread didn't die"
    waitUntilDead n = do
      s <- threadStatus tid
      if s == ThreadFinished || s == ThreadDied
      then return ()
      else do
        threadDelay 1000
        waitUntilDead (n - 1)
