module GitFile
( GitFile
, createGitFile
, addToGitFile
, getGitFilePath
, getGitFileCommits
, getGitFileStats
, gitFileToStatLine
) where

import Commit
import Util (addUnique)

data GitFile = GitFile { path :: String
                       , commits :: [Commit]
                       , authors :: [String]
                       , additions :: Int
                       , deletions :: Int
                       } deriving (Show)

createGitFile :: String -> GitFile
createGitFile p = GitFile { path=p, commits=[], authors=[], additions=0, deletions=0 }

addToGitFile :: Commit -> Int -> Int -> GitFile -> GitFile
addToGitFile c a d f = GitFile { path=(path f)
                               , commits=c:commits f
                               , authors=addUnique (getCommitAuthor c) (authors f)
                               , additions=nextA
                               , deletions=nextD
                               }
  where nextA = (additions f) + a
        nextD = (deletions f) + d


getGitFileCommits :: GitFile -> [Commit]
getGitFileCommits = commits

getGitFilePath :: GitFile -> String
getGitFilePath = path

getGitFileStats :: GitFile -> (Int, Int, Int, Int)
getGitFileStats f = (com f, auth f, additions f, deletions f)
  where com = length . commits
        auth = length . authors


gitFileToStatLine :: GitFile -> [String]
gitFileToStatLine f = (path f):(toStats (getGitFileStats f))
  where toStats (a, b, c, d) = map show [a, b, c, d,  c - d]
