module Main where

import           Data.Char
import           Data.Either
import           Data.List
import           Data.Maybe
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Test.Tasty             (defaultMain, testGroup, localOption)
import           Test.Tasty.QuickCheck

main = defaultMain $ testGroup "All tests" [
    testProperty "Simple types"  canReadSimpleTypes
  , testProperty "Applied types" canReadAppliedTypes
  ]

canReadSimpleTypes = False

canReadAppliedTypes = False
