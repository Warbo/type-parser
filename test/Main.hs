module Main where

import           Parse
import           Test.QuickCheck
import           Test.Tasty             (defaultMain, testGroup, localOption)
import           Test.Tasty.QuickCheck

main = defaultMain $ testGroup "All tests" [
    testProperty "Simple types"  canReadSimpleTypes
  , testProperty "Applied types" canReadAppliedTypes
  ]

canReadSimpleTypes (TyCon s) = parseType s === Just (Leaf s)

canReadAppliedTypes = False

newtype TyCon = TyCon String deriving (Show)

instance Arbitrary TyCon where
  arbitrary = do initial <- elements upper
                 rest    <- listOf (elements alpha)
                 return (TyCon (initial:rest))

upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
lower = "abcdefghijklmnopqrstuvwxyz"
alpha = upper ++ lower
