module Main (main) where

import Test.Tasty
import Test.Tasty.QuickCheck

import Queue.Test
import Example.DieHard
import Example.TicketDispenser
import Registry.Test

------------------------------------------------------------------------

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ testProperty "Queue" prop_queue
  , testProperty "DieHard" (expectFailure prop_dieHard)
  , testProperty "Registry" prop_registry
  , testProperty "TicketDispenserSeq" prop_ticketDispenserSeq
  , testProperty "TicketDispenserPar" prop_ticketDispenserPar
  ]
