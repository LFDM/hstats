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

gitSubDirToDir :: Bool -> GitDir -> GitDir
gitSubDirToDir isDirectChild s = GitDir { path=joinPath . init . splitDirectories . path $ s
                          , commits=commits s
                          , authors=authors s
                          , additions=additions s
                          , deletions=deletions s
                          , children= if isDirectChild then [s] else []
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
collectDirs :: [GitFile] -> GitDir
collectDirs = toDirTree . Prelude.foldr collectDirsFromFile Map.empty

collectDirsFromFile :: GitFile -> Map String GitDir -> Map String GitDir
collectDirsFromFile f = addDir pDirs f
  where pDirs = init . splitDirectories . getGitFilePath $ f
        addDir [] _ z = z
        addDir ps y z = addDir (init ps) y $ insert (joinPath ps) y z
        insert k y z = Map.insert k (merge k y z) z
        merge k y z = addToGitDir y $ lookupWithDefault (empty k) k z
        empty = createGitDir


toDirTree :: Map String GitDir -> GitDir
toDirTree ds = unpack $ List.foldr addParents ds (Map.toList ds)
  where empty = createGitDir ""
        unpack = lookupWithDefault empty ""

addParents :: (String, GitDir) -> Map String GitDir -> Map String GitDir
addParents (p, d) = addToParents' True pPaths d
  where pPaths = "":splitDirectories p

addToParents' :: Bool -> [String] -> GitDir -> Map String GitDir -> Map String GitDir
addToParents' isDirectChild [] _ ds = ds
addToParents' isDirectChild xs s ds = addToParents' False pPaths s $ next ds
  where pPaths = init xs
        next = Map.insertWith mergeGitDir (concat pPaths) (gitSubDirToDir isDirectChild s)

