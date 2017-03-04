import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified Test.Tasty.SmallCheck as SC

import qualified UtilTest as UtilTest

arith :: Integer -> Integer -> Property
arith x y = (x > 0) && (y > 0) ==> (x+y)^2 > x^2 + y^2

negation :: Integer -> Bool
negation x = abs (x^2) >= x

units = testGroup "Units"
  [ testCase "Equality" $ True @=? True
  , testCase "Assertion" $ assert $ (length [1,2,3]) == 3
  ]

quickchecks = testGroup "QuickCheck tests"
  [ testProperty "Quickcheck test" arith
  ]

smallchecks = testGroup "SmallCheck tests"
  [ SC.testProperty "Negation" negation
  ]

suites = [ units
         , quickchecks
         , smallchecks
         , UtilTest.suite
         ]

suite :: TestTree
suite = testGroup "hstats" suites

main :: IO ()
main = defaultMain suite
