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
                         , commits=mergeUnique (commits x) (commits y)
                         , authors=mergeUnique (authors x) (authors y)
                         , additions=sum . map additions $ [x, y]
                         , deletions=sum . map deletions $ [x, y]
                         , children=mergeUnique (children x) (children y)
                         }

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
                          , commits=mergeUnique (commits par) (commits sub)
                          , authors=mergeUnique (authors par) (authors sub)
                          , additions= sum . map additions $ [par, sub]
                          , deletions= sum . map deletions $ [par, sub]
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
gitDirToStatLine d = (path d):foldr showStat [] [(length .commits), additions, deletions]
  where showStat x y = (show . x) d:y


