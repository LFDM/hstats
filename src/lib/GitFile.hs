module GitFile
( GitFile
, createGitFile
, addToGitFile
, getGitFilePath
, getGitFileAuthors
, getGitFileCommits
, getGitFileStats
, getGitFileAdditions
, getGitFileDeletions
, gitFileToStatLine
, sortGitFilesByCommits
) where

import Commit
import Util (addUnique, sortByAccessorDesc)

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

getGitFileAuthors :: GitFile -> [String]
getGitFileAuthors = authors

getGitFilePath :: GitFile -> String
getGitFilePath = path

getGitFileStats :: GitFile -> (Int, Int, Int, Int)
getGitFileStats f = (com f, auth f, additions f, deletions f)
  where com = length . commits
        auth = length . authors

getGitFileAdditions :: GitFile -> Int
getGitFileAdditions = additions

getGitFileDeletions :: GitFile -> Int
getGitFileDeletions = deletions

gitFileToStatLine :: GitFile -> [String]
gitFileToStatLine f = (path f):(toStats (getGitFileStats f))
  where toStats (a, b, c, d) = map show [a, b, c, d,  c - d, avg a c, avg a d, avg a (c - d)]
        avg 0 y = 0
        avg x y = y `quot` x

sortGitFilesByCommits :: [GitFile] -> [GitFile]
sortGitFilesByCommits = sortByAccessorDesc $ length . commits

