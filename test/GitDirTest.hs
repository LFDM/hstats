module GitDirTest
( suite
) where

import Test.Tasty
import Test.Tasty.HUnit

import GitDir
import GitFile

import Data.Map as Map

a11 = createGitFile' "a/1/test1.hs" [] [] 1 10
a12 = createGitFile' "a/1/test2.hs" [] [] 2 20
a23 = createGitFile' "a/2/test3.hs" [] [] 4 40
a24 = createGitFile' "a/2/test4.hs" [] [] 8 80
b15 = createGitFile' "b/1/test5.hs" [] [] 16 160

files = [a11, a12, a23, a24, b15]

suite :: TestTree
suite = testGroup "GitDir"
  [ testGroup "collectDirs"
    [ testCase "ends up with a root dir" $ gitDirRootTest
    , testCase "collects all additions for the root" $ gitDirRootAddTest
    , testCase "collects all deletions for the root" $ gitDirRootDelTest
    , testCase "collects all root children I" $ gitDirRootChildrenLengthTest
    , testCase "collects all root children II" $ gitDirRootChildrenPathTest
    , testCase "collects all root children III" $ gitDirRootChildrenAdditionTest
    , testCase "collects subdirectories of children I" $ gitDirRootGrandchildrenLengthTest
    , testCase "collects subdirectories of children II" $ gitDirRootGrandchildrenPathTest
    ]
  , testGroup "collectDirFromFile"
    [ testCase "converts file to dir I" $ collectDirFromFileLengthTest
    , testCase "converts file to dir II" $ collectDirFromFilePathTest
    , testCase "merges with an already present dir I" $ collectDirFromFilePathWithMergeTest
    , testCase "merges with an already present dir II" $ collectDirFromFileAdditionsWithMergeTest
    , testCase "merges with an already present dir III" $ collectDirFromFileDeletionsWithMergeTest
    ]
  ]

collectDirs' = collectDirs ""
gitDirRootTest = "root" @=? actual
  where actual = getGitDirPath $ collectDirs "root" files

gitDirRootAddTest = 31 @=? actual
  where actual = getGitDirAdditions $ collectDirs' files

gitDirRootDelTest = 310 @=? actual
  where actual = getGitDirDeletions $ collectDirs' files

gitDirRootChildrenLengthTest = 2 @=? actual
  where actual = length . getGitDirChildren . collectDirs' $ files

gitDirRootChildrenPathTest = ["b", "a"] @=? actual
  where actual = toPaths  . getGitDirChildren . collectDirs' $ files

gitDirRootChildrenAdditionTest = 15 @=? actual
  where actual = getGitDirAdditions . takeSecond . getGitDirChildren . collectDirs' $ files

gitDirRootGrandchildrenLengthTest = 2 @=? actual
  where actual = length . (getGrandchildren takeSecond) . collectDirs' $ files

gitDirRootGrandchildrenPathTest = ["a/2", "a/1"] @=? actual
  where actual = toPaths . (getGrandchildren takeSecond) . collectDirs' $ files

collectDirFromFileLengthTest = 1 @=? actual
  where actual = length . Map.toList $ collectDirFromFile a11 Map.empty

collectDirFromFilePathTest = ["a/1"] @=? actual
  where actual = (Prelude.map fst) . Map.toList $ collectDirFromFile a11 Map.empty

collectDirFromFilePathWithMergeTest = ["a/1"] @=? actual
  where actual = (Prelude.map fst) . Map.toList $ collected
        collected = Prelude.foldr collectDirFromFile Map.empty [a11, a12]

collectDirFromFileAdditionsWithMergeTest = 3 @=? actual
  where actual = getGitDirAdditions . snd . head . Map.toList $ collected
        collected = Prelude.foldr collectDirFromFile Map.empty [a11, a12]

collectDirFromFileDeletionsWithMergeTest = 30 @=? actual
  where actual = getGitDirDeletions . snd . head . Map.toList $ collected
        collected = Prelude.foldr collectDirFromFile Map.empty [a11, a12]

getGrandchildren :: ([GitDir] -> GitDir) -> GitDir -> [GitDir]
getGrandchildren childAccessor = getGitDirChildren . childAccessor . getGitDirChildren

takeSecond :: [a] -> a
takeSecond = head . tail

toPaths :: [GitDir] -> [String]
toPaths = Prelude.map getGitDirPath

