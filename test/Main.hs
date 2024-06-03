module Main (main) where

import Test.Tasty
import Test.Tasty.QuickCheck

import Example.Counter
import Example.Queue.Test
import Example.DieHard
import Example.TicketDispenser
import Example.Registry.Test
import Example.FileSystem.Test

------------------------------------------------------------------------

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ sequentialTestGroup "Counter" AllSucceed
      [ testProperty "Sequential" prop_counter
      , testProperty "Parallel" (expectFailure prop_parallelCounter)
      ]
  , testProperty "Queue" prop_queue
  , testProperty "DieHard" (expectFailure prop_dieHard)
  , sequentialTestGroup "Registry" AllSucceed
      [ testProperty "Sequential" prop_registry
      , testProperty "Parallel" prop_parallelRegistry
      ]
  , testGroup "TicketDispenser"
      [ testProperty "Sequential" prop_ticketDispenser
      , testProperty "Parallel" (expectFailure prop_parallelTicketDispenser)
      ]
  , testProperty "FileSystem" prop_fileSystem
  ]
