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

data GitFile = GitFile { path :: String
                       , commits :: [Commit]
                       , additions :: Int
                       , deletions :: Int
                       } deriving (Show)

createGitFile :: String -> GitFile
createGitFile p = GitFile { path=p, commits=[], additions=0, deletions=0 }

addToGitFile :: Commit -> Int -> Int -> GitFile -> GitFile
addToGitFile c a d f = GitFile { path=(path f), commits=c:commits f, additions=nextA, deletions=nextD }
  where nextA = (additions f) + a
        nextD = (deletions f) + d

getGitFileCommits :: GitFile -> [Commit]
getGitFileCommits = commits

getGitFilePath :: GitFile -> String
getGitFilePath = path

getGitFileStats :: GitFile -> (Int, Int, Int)
getGitFileStats f = (com f, additions f, deletions f)
  where com = length . commits


gitFileToStatLine :: GitFile -> [String]
gitFileToStatLine f = (path f):(toStats (getGitFileStats f))
  where toStats (a, b, c) = map show [a, b, c,  b - c]
