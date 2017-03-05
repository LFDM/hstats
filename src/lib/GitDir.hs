module GitDir
( GitDir
, gitSubDirToDir
, getGitDirPath
, getGitDirChildren
, getGitDirAuthors
, getGitDirAdditions
, getGitDirDeletions
, getGitDirCommits , mergeGitDir , gitDirToStatLine
, gitDirToNormalizedSortedList
, collectDirs
, collectDirFromFile
) where

import Data.Function
import Data.Map as Map
import Data.List as List
import System.FilePath

import Commit
import GitFile
import Util (lookupWithDefault, mergeUnique, sortByAccessorDesc)

data GitDir = GitDir { path :: String
                     , commits :: [Commit]
                     , authors :: [String]
                     , additions :: Int
                     , deletions :: Int
                     , children :: [GitDir]
                     } deriving (Show)

instance Eq GitDir where
  x == y = ((==) `on` path) x y

createGitDir :: String -> GitDir
createGitDir p = GitDir { path=p
                        , commits=[]
                        , authors=[]
                        , additions=0
                        , deletions=0
                        , children=[]
                        }

mergeGitDir :: GitDir -> GitDir -> GitDir
mergeGitDir x y = GitDir { path=path x
                         , commits=mergeCommits x y
                         , authors=mergeAuthors x y
                         , additions=sumMap additions [x, y]
                         , deletions=sumMap deletions [x, y]
                         , children=mergeUnique (children x) (children y)
                         }
  where mergeCommits = mergeUnique `on` commits
        mergeAuthors = mergeUnique `on` authors

addToGitDir :: GitFile -> GitDir -> GitDir
addToGitDir f d = GitDir { path=path d
                         , commits=mergeUnique (getGitFileCommits f) (commits d)
                         , authors=mergeUnique (getGitFileAuthors f) (authors d)
                         , additions=getGitFileAdditions f + additions d
                         , deletions=getGitFileDeletions f + deletions d
                         , children=children d
                         }

addGitSubDir :: GitDir -> GitDir -> GitDir
addGitSubDir par sub = GitDir { path=path par
                          , commits=mergeCommits par sub
                          , authors=mergeAuthors par sub
                          , additions= sumMap additions [par, sub]
                          , deletions= sumMap deletions [par, sub]
                          , children=sub:children par
                          }

addGitSubDir' sub par = addGitSubDir par sub

gitSubDirToDir :: GitDir -> GitDir
gitSubDirToDir s = GitDir { path=joinPath . init . splitDirectories . path $ s
                          , commits=commits s
                          , authors=authors s
                          , additions=additions s
                          , deletions=deletions s
                          , children= [s]
                          }

getGitDirPath = path
getGitDirChildren = children
getGitDirAuthors = children
getGitDirAdditions = additions
getGitDirDeletions = deletions
getGitDirCommits = commits

gitDirToStatLine :: GitDir -> [String]
gitDirToStatLine d = (path d):List.foldr showStat [] accs
  where showStat x y = (show . x) d:y
        accs = [(length . commits), (length . authors), additions, deletions, diff]
        diff x = additions x - deletions x

mergeCommits = mergeUnique `on` commits
mergeAuthors = mergeUnique `on` authors

sumMap f = sum . List.map f

gitDirToNormalizedSortedList :: GitDir -> [(Maybe GitDir, GitDir)]
gitDirToNormalizedSortedList = gitDirToNormalizedSortedList' Nothing



gitDirToNormalizedSortedList' :: Maybe GitDir -> GitDir -> [(Maybe GitDir, GitDir)]
gitDirToNormalizedSortedList' par d = withChildren par d $ sortGitDirsByCommits (children d)
  where withChildren p x [] = concat [[(p, x)], [(Nothing, createGitDir "YYY")]]
        withChildren p x (y:[]) = gitDirToNormalizedSortedList' p y
        withChildren p x ys = (p, x):concat [concatMap (gitDirToNormalizedSortedList' (Just x)) ys, [(Nothing, createGitDir "XXX")]]

sortGitDirsByCommits :: [GitDir] -> [GitDir]
sortGitDirsByCommits = sortByAccessorDesc $ length . commits


-- while we could create the tree on the fly, it's cheaper to
-- do a second pass, otherwise we create a lot of intermediary objects
-- We therefore just create all directories that contain actual files and
-- then organize them as a tree
collectDirs :: [GitFile] -> GitDir
collectDirs = toDirTree . Prelude.foldr collectDirFromFile Map.empty

collectDirFromFile :: GitFile -> Map String GitDir -> Map String GitDir
collectDirFromFile f = insert (dir f)
  where dir = joinPath . init . splitDirectories . getGitFilePath
        insert k ds = Map.insert k (merge k ds) ds
        merge k ds = addToGitDir f $ lookupWithDefault (empty k) k ds
        empty = createGitDir

-- a/1
-- b/2
--
  -- take a -> merge with subdir of a/1

toDirTree :: Map String GitDir -> GitDir
toDirTree ds = unpack $ List.foldr addToParents ds toSplitKeys
  where empty = createGitDir ""
        unpack = lookupWithDefault empty ""
        toSplitKeys = List.map (\p -> "":splitDirectories p) $ keys ds

-- "", "a", "b", "c"
addToParents :: [String] -> Map String GitDir -> Map String GitDir
addToParents [] ds = ds
addToParents (x:[]) ds = ds
addToParents xs ds = addToParents pPaths $ next ds
  where pPaths = init xs
        currentPath = joinPath xs
        nextPath = joinPath pPaths
        dir = lookupWithDefault (createGitDir currentPath) currentPath ds
        next = Map.insertWith mergeGitDir nextPath (gitSubDirToDir dir)

