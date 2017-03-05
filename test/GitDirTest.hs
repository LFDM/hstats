module GitDirTest
( suite
) where

import Test.Tasty
import Test.Tasty.HUnit

import GitDir
import GitFile

a11 = createGitFile' "a/1/test1.hs" [] [] 1 10
a12 = createGitFile' "a/1/test2.hs" [] [] 2 20
a23 = createGitFile' "a/2/test3.hs" [] [] 4 40
b14 = createGitFile' "b/1/test4.hs" [] [] 8 80

files = [a11, a12, a23, b14]

suite :: TestTree
suite = testGroup "GitDir"
  [ testGroup "collectDirs"
    [ testCase "ends up with a root dir" $ gitDirRootTest
    , testCase "collects all additions for the root" $ gitDirRootAddTest
    , testCase "collects all deletions for the root" $ gitDirRootDelTest
    , testCase "collects all root children I" $ gitDirRootChildrenLengthTest
    , testCase "collects all root children II" $ gitDirRootChildrenPathTest
    , testCase "collects subdirectories of children I" $ gitDirRootGrandchildrenLengthTest
    , testCase "collects subdirectories of children II" $ gitDirRootGrandchildrenPathTest
    ]
  ]

gitDirRootTest = "" @=? actual
  where actual = getGitDirPath $ collectDirs files

gitDirRootAddTest = 15 @=? actual
  where actual = getGitDirAdditions $ collectDirs files

gitDirRootDelTest = 150 @=? actual
  where actual = getGitDirDeletions $ collectDirs files

gitDirRootChildrenLengthTest = 2 @=? actual
  where actual = length . getGitDirChildren . collectDirs $ files

gitDirRootChildrenPathTest = ["b", "a"] @=? actual
  where actual = toPaths  . getGitDirChildren . collectDirs $ files

gitDirRootGrandchildrenLengthTest = 2 @=? actual
  where actual = length . (getGrandchildren takeSecond) . collectDirs $ files

gitDirRootGrandchildrenPathTest = ["a/2", "a/1"] @=? actual
  where actual = toPaths . (getGrandchildren takeSecond) . collectDirs $ files

getGrandchildren :: ([GitDir] -> GitDir) -> GitDir -> [GitDir]
getGrandchildren childAccessor = getGitDirChildren . childAccessor . getGitDirChildren

takeSecond :: [a] -> a
takeSecond = head . tail

toPaths :: [GitDir] -> [String]
toPaths = map getGitDirPath

