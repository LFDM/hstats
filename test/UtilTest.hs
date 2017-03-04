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

  , testGroup "addUnique"
    [ testCase "adds element to a list when it's not already present" $ (addUnique 1 [2]) @=? [1, 2]
    , testCase "doesn't add element when already present" $ (addUnique 1 [1]) @=? [1]
    ]

  , testGroup "removeLeading"
    [ testCase "removes a leading string" $ (removeLeading "abc" "abcde") @=? "de"
    ]

  , testGroup "replaceLeading"
    [ testCase "replaces leading elemnts in a list" $ (replaceLeading "abc" " " "abcde") @=? "   de"
    ]
  ]
