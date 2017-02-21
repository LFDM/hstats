module GitDir
( GitDir
, createGitDir
, addToGitDir
, addGitSubDir
, addGitSubDir'
, gitSubDirToDir
, getGitDirPath
, getGitDirChildren
, getGitDirAuthors
, getGitDirAdditions
, getGitDirDeletions
, getGitDirCommits
, mergeGitDir
, gitDirToStatLine
) where

import Data.Function
import System.FilePath

import Commit
import GitFile
import Util (mergeUnique)

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
                          , children=s:children s
                          }

getGitDirPath = path
getGitDirChildren = children
getGitDirAuthors = children
getGitDirAdditions = additions
getGitDirDeletions = deletions
getGitDirCommits = commits

gitDirToStatLine :: GitDir -> [String]
gitDirToStatLine d = (path d):foldr showStat [] accs
  where showStat x y = (show . x) d:y
        accs = [(length .commits), (length . authors), additions, deletions]

mergeCommits = mergeUnique `on` commits
mergeAuthors = mergeUnique `on` authors

sumMap f = sum . map f
