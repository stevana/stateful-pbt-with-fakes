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
  [ testProperty "Counter" prop_counter
  , testProperty "ParallelCounter" (expectFailure prop_parallelCounter)
  , testProperty "Queue" prop_queue
  , testProperty "DieHard" (expectFailure prop_dieHard)
  , testProperty "Registry" prop_registry
  , testProperty "ParallelRegistry" (noShrinking $ prop_parallelRegistry)
  , testProperty "Unregister" (prop_unregister)
  , testProperty "Race" (prop_race)
  , testProperty "ShouldNotHaveBeenGenerated" prop_shouldNotHaveBeenGenerated
  , testProperty "ParallelGeneration" prop_parallelGeneration
  , testProperty "TicketDispenser" prop_ticketDispenser
  , testProperty "ParallelTicketDispenser" (expectFailure prop_parallelTicketDispenser)
  , testProperty "FileSystem" prop_fileSystem
  ]
