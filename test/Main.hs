module Main where

import           Data.List
import           Data.Maybe
import           Parse
import           Test.QuickCheck
import           Test.Tasty             (defaultMain, testGroup, localOption)
import           Test.Tasty.QuickCheck

main = defaultMain $ testGroup "All tests" [
    testProperty "Simple types have no args"         simpleTypesNoArgs
  , testProperty "Simple functions have simple args" simpleFunctionArgs
  , testProperty "Can split up function types"       canSplitFunTypes
  ]

simpleTypesNoArgs t = parseArgs (type2String (TC t)) === []

simpleFunctionArgs (Positive n) (NonEmpty ts) =
    counterexample (show (("funType", funType),
                          ("strBits", show strBits),
                          ("found",   show found)))
                   test
  where bits    = take (n+1) (cycle ts)
        strBits = map (type2String . TC) bits
        funType = intercalate " -> " strBits
        expect  = init strBits
        found   = map pp (parseArgs funType)
        gotExpected  = all (`elem` found)  expect
        noUnexpected = all (`elem` expect) found
        test = gotExpected && noUnexpected

canSplitFunTypes (NonEmpty ts) =
    counterexample (show (("bits",   show bits),
                          ("funT",   show funT),
                          ("foundS", show foundS)))
                   test
  where bits = map type2String ts
        funT = intercalate " -> " bits
        parsed = fromJust (parseType funT)
        found  = splitType parsed
        foundS = map pp found
        gotExpected  = all (`elem` bits)   foundS
        noUnexpected = all (`elem` foundS) bits
        test         = gotExpected && noUnexpected

type2String :: Type -> String
type2String (TC (TyCon t))  = t
type2String (TA (TyApp ts)) = "((" ++ intercalate ") ("    (map type2String ts) ++ "))"
type2String (TF (TyFun ts)) = "((" ++ intercalate ") -> (" (map type2String ts) ++ "))"

newtype TyCon = TyCon String deriving (Show)
newtype TyApp = TyApp [Type] deriving (Show)
newtype TyFun = TyFun [Type] deriving (Show)

data Type = TC TyCon | TA TyApp | TF TyFun deriving (Show)

instance Arbitrary TyCon where
  arbitrary = do initial <- elements upper
                 rest    <- listOf (elements alpha)
                 return (TyCon (initial:rest))

upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
lower = "abcdefghijklmnopqrstuvwxyz"
alpha = upper ++ lower

sizedList g 0 = do x <- g 0
                   return [x]
sizedList g n = do i  <- choose (1,n)
                   x  <- g i
                   xs <- sizedList g (n-i)
                   return (x:xs)

sizedTyApp :: Int -> Gen TyApp
sizedTyApp n = TyApp <$> sizedList sizedType n

sizedTyFun n = TyFun <$> sizedList sizedType n

sizedType 0 = TC <$> arbitrary
sizedType n = oneof [TC <$> arbitrary, TA <$> sizedTyApp n, TF <$> sizedTyFun n]

instance Arbitrary TyApp where
  arbitrary = do n <- choose (0, 100)
                 sizedTyApp n

instance Arbitrary Type where
  arbitrary = do n <- choose (0, 100)
                 sizedType n
