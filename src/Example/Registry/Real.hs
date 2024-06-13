-- This code is mostly taken from John Hughes' Midlands Graduate School 2019
-- course:
--
--   https://www.cse.chalmers.se/~rjmh/MGS2019/

-- A simple local name service for threads... behaves like the Erlang
-- process registry.

-- If you are doing the associated QuickCheck exercises, DO NOT READ
-- THIS CODE!!! The exercise is one in black box testing.


module Example.Registry.Real where

import Control.Concurrent
import Control.Monad
-- import Data.IORef
import SleepyIORef
import GHC.Conc (ThreadStatus(ThreadDied, ThreadFinished), threadStatus)
import System.IO.Unsafe

-- start snippet RegistryRealRace
alive :: ThreadId -> IO Bool
alive tid = do
  s <- threadStatus tid
  return $ s /= ThreadFinished && s /= ThreadDied

{-# NOINLINE registry #-}
registry :: IORef [(String,ThreadId)]
registry = unsafePerformIO (newIORef [])

spawn :: IO ThreadId
spawn = forkIO (threadDelay 100000000)

whereis :: String -> IO (Maybe ThreadId)
whereis name = do
  reg <- readRegistry
  return $ lookup name reg

register :: String -> ThreadId -> IO ()
register name tid = do
  ok <- alive tid
  reg <- readRegistry
  if ok && name `notElem` map fst reg && tid `notElem` map snd reg
    then do
      atomicModifyIORef registry $ \reg' ->
           if name `notElem` map fst reg' && tid `notElem` map snd reg'
             then ((name,tid):reg',())
             else (reg',badarg)
    else badarg

unregister :: String -> IO ()
unregister name = do
  reg <- readRegistry
  if name `elem` map fst reg
    then atomicModifyIORef registry $ \reg' ->
           (filter ((/=name).fst) reg',
            ())
    else badarg

readRegistry :: IO [(String, ThreadId)]
readRegistry = do
  reg <- readIORef registry
  garbage <- filterM (fmap not.alive) (map snd reg)
  atomicModifyIORef' registry $ \reg' ->
    let reg'' = filter ((`notElem` garbage).snd) reg' in (reg'',reg'')

badarg :: a
badarg = error "bad argument"

kill :: ThreadId -> IO ()
kill tid = do
  killThread tid
  waitUntilDead 1000
  where
    waitUntilDead :: Int -> IO ()
    waitUntilDead 0 = error "kill: thread didn't die"
    waitUntilDead n = do
      b <- alive tid
      if b
      then do
        threadDelay 1000
        waitUntilDead (n - 1)
      else return ()
-- end snippet RegistryRealRace

------------------------------------------------------------------------

-- start snippet registerNoRace
{-# NOINLINE lock #-}
lock :: MVar ()
lock = unsafePerformIO (newMVar ())

registerNoRace :: String -> ThreadId -> IO ()
registerNoRace name tid = withMVar lock $ \_ -> register name tid
-- end snippet registerNoRace

unregisterNoRace :: String -> IO ()
unregisterNoRace name = withMVar lock $ \_ -> unregister name

killNoRace :: ThreadId -> IO ()
killNoRace tid = withMVar lock $ \_ -> kill tid

registerBug :: String -> ThreadId -> IO ()
registerBug name tid = do
  ok <- alive tid
  reg <- readRegistry
  if ok && name `notElem` map fst reg && tid `notElem` map snd reg
    then atomicModifyIORef' registry $ \reg' ->
           if name `notElem` map fst reg' && tid `notElem` map snd reg'
             then ([(name,tid)],())
             else (reg',badarg)
    else badarg
