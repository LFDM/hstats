module UtilTest
( suite
) where

import Test.Tasty
import Test.Tasty.HUnit

import Util

suite :: TestTree
suite = testGroup "Util"
  [ testGroup "trimR"
    [ testCase "trims spaces on the right side" $ (trimR " a  ") @=? " a"
    ]

  , testGroup "trimL"
    [ testCase "trims spaces on the left side" $ (trimL " a  ") @=? "a  "
    ]
  , testGroup "trim"
    [ testCase "trims spaces on the both sides" $ (trim " a  ") @=? "a"
    ]

  , testGroup "shorten"
    [ testCase "shortens a string to a given length I" $ (shorten 2 "12345") @=? "45"
    , testCase "shortens a string to a given length II" $ (shorten 8 "12345") @=? "12345"
    ]
  ]
