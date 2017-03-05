module GitDir
( GitDir
, getGitDirPath
, getGitDirChildren
, getGitDirAuthors
, getGitDirAdditions
, getGitDirDeletions
, getGitDirCommits , mergeGitDir , gitDirToStatLine
, gitDirToNormalizedSortedList
, gitDirToSortedPathTree
, collectDirs
, collectDirFromFile
) where

import Data.Function
import Data.Map as Map
import Data.List as List
import System.FilePath

import Commit
import GitFile
import Util (lookupWithDefault, mergeUnique, sortByAccessorDesc, removeLeading, removeLeadingSlash, StringTree(..))

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

gitDirToNormalizedSortedList :: GitDir -> [GitDir]
gitDirToNormalizedSortedList d = withChildren d $ sortGitDirsByCommits (children d)
  where withChildren x [] = [x]
        withChildren x ys = x:concatMap gitDirToNormalizedSortedList ys

gitDirToSortedPathTree :: String -> GitDir -> StringTree
gitDirToSortedPathTree parentPath d = STN (toPath, List.map (gitDirToSortedPathTree (path d)) cs)
  where cs = sortGitDirsByCommits $ children d
        toPath = removeLeadingSlash . (removeLeading parentPath) $ path d

sortGitDirsByCommits :: [GitDir] -> [GitDir]
sortGitDirsByCommits = sortByAccessorDesc $ length . commits


-- while we could create the tree on the fly, it's cheaper to
-- do a second pass, otherwise we create a lot of intermediary objects
-- We therefore just create all directories that contain actual files and
-- then organize them as a tree
collectDirs :: String -> [GitFile] -> GitDir
collectDirs root = (toDirTree root) . Prelude.foldr collectDirFromFile Map.empty

collectDirFromFile :: GitFile -> Map String GitDir -> Map String GitDir
collectDirFromFile f = insert (dir f)
  where dir = joinPath . init . splitDirectories . getGitFilePath
        insert k ds = Map.insert k (merge k ds) ds
        merge k ds = addToGitDir f $ lookupWithDefault (empty k) k ds
        empty = createGitDir

toDirTree :: String -> Map String GitDir -> GitDir
toDirTree root ds = unpack $ List.foldr addToParents ds (Map.toList ds)
  where empty = createGitDir root
        unpack = lookupWithDefault empty root

addToParents :: (String, GitDir) -> Map String GitDir -> Map String GitDir
addToParents (p, d) = addToParents' pPaths True d
  where pPaths = "":splitDirectories p

addToParents' :: [String] -> Bool -> GitDir -> Map String GitDir -> Map String GitDir
addToParents' [] _ _ ds = ds
addToParents' (x:[]) _ _ ds = ds
addToParents' xs isDirectChild d ds = addToParents' pPaths False d $ next ds
  where pPaths = init xs
        nextPath = joinPath pPaths
        currentPath = joinPath xs
        currentDir = Map.lookup currentPath ds
        currentChildren Nothing = []
        currentChildren (Just child) = [child]
        next = Map.insertWith mergeGitDir nextPath subDir
        subDir = GitDir { path=nextPath
                        , commits=commits d
                        , authors=authors d
                        , additions=additions d
                        , deletions=deletions d
                        , children=currentChildren currentDir
                        }

