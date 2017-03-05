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
  , testGroup "renderAsTree"
    [ testCase "formats strings in a nice tree shape 1" $ renderAsTreeTest1
    , testCase "formats strings in a nice tree shape 2" $ renderAsTreeTest2
    , testCase "formats strings in a nice tree shape 3" $ renderAsTreeTest3
    , testCase "formats strings in a nice tree shape 4" $ renderAsTreeTest4
    ]
  ]

renderAsTreeTest1 = actual @=? expected
  where actual = ["x"]
        input = STN ("x", [])
        expected = renderAsTree input

renderAsTreeTest2 = actual @=? expected
  where actual = [ "x"
                 , "└ a"
                 ]
        input = STN ("x", [STN ("a", [])])
        expected = renderAsTree input

renderAsTreeTest3 = actual @=? expected
  where actual = [ "x"
                 , "└ a"
                 , "  ├ 1"
                 , "  │ └ A"
                 , "  └ 2"
                 ]
        input = STN ("x", [STN ("a", [STN ("1", [STN ("A", [])]), STN ("2", [])])])
        expected = renderAsTree input

renderAsTreeTest4 = actual @=? expected
  where actual = [ "x"
                 , "├ a"
                 , "│ ├ 1"
                 , "│ │ └ A"
                 , "│ └ 2"
                 , "│   └ B"
                 , "└ b"
                 ]
        input = STN ("x", [STN ("a", [STN ("1", [STN ("A", [])]), STN ("2", [STN ("B", [])])]), STN ("b", []) ])
        expected = renderAsTree input

