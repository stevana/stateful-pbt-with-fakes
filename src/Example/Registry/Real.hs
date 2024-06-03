-- A simple local name service for threads... behaves like the Erlang
-- process registry.

-- If you are doing the associated QuickCheck exercises, DO NOT READ
-- THIS CODE!!! The exercise is one in black box testing.


module Example.Registry.Real where

import Control.Concurrent
import Control.Monad
import Data.IORef
import GHC.Conc (ThreadStatus(ThreadDied, ThreadFinished), threadStatus)
import System.IO.Unsafe

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

spawn :: IO ThreadId
spawn = forkIO (threadDelay 100000000)

whereis :: String -> IO (Maybe ThreadId)
whereis name = do
  reg <- readRegistry
  return $ lookup name reg

register :: String -> ThreadId -> IO ()
register name tid =
  withMVar lock $ \_ -> do
    ok <- alive tid
    threadDelay 1000
    reg <- readRegistry
    threadDelay 1000
    if ok && name `notElem` map fst reg && tid `notElem` map snd reg
      then do
        threadDelay 1000
        atomicModifyIORef registry $ \reg' ->
             if name `notElem` map fst reg' && tid `notElem` map snd reg'
               then ((name,tid):reg',())
               else (reg',badarg)
      else badarg

unregister :: String -> IO ()
unregister name =
  withMVar lock $ \_ -> do
    reg <- readRegistry
    threadDelay 1000
    if name `elem` map fst reg
      then atomicModifyIORef registry $ \reg' ->
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
