{-# LANGUAGE FlexibleContexts #-}

module Hot
( printStats
) where

import System.Process
import System.IO
import Text.Regex.Posix

import Printer as P

data Commit = Commit { author :: String
                     , sha :: String
                     , date :: String
                     , files :: [FileStat]
                     } deriving (Show)

data FileStat = FileStat { path :: FilePath
                         , additions :: Int
                         , deletions :: Int
                         } deriving (Show)

type GitLineData = (String, String, String, [FileStat])
type State = String
type GitState = (State, GitLineData, [Commit])
type Line = String

emptyLineData = ("", "", "", [])

lineDataToCommit :: GitLineData -> Commit
lineDataToCommit (sha, author, date, files) = Commit { sha=sha
                                                     , author=author
                                                     , date=date
                                                     , files=files
                                                     }

toStat :: String -> String -> String -> FileStat
toStat a d p = FileStat {additions=read a :: Int, deletions=read d :: Int, path=p}

gitLog :: String -> IO String
gitLog timeframe = readProcess cmd args []
  where cmd = "git"
        args = ["log", "--numstat", "--no-color", "--since=" ++ timeframe]

printStats :: String -> IO ()
printStats timeframe = do
  gitOutput <- gitLog timeframe
  print $ (parseGitOutput gitOutput)!!0

parseGitOutput :: String -> [Commit]
parseGitOutput = reverse . takeResult . processLines . lines
  where takeResult (_, _, x) = x

flushState :: GitState -> GitState
flushState all@(s, d@(sha, _, _, _), res) =
  if null sha then all else ("BEGIN", emptyLineData, addRecord d)
   where addRecord lineData = (lineDataToCommit lineData):res

processLines :: [Line] -> GitState
processLines = foldl processLine ("BEGIN", emptyLineData, [])

processLine :: GitState -> Line -> GitState
processLine x@("BEGIN", _, _) = processCommit x "AUTHOR"
processLine x@("AUTHOR", _, _) = processAuthor x "DATE"
processLine x@("DATE", _, _) = processDate x "STAT_BEGIN"
processLine x@("STAT_BEGIN", _, _) = processStatBegin x "STAT"
processLine x@("STAT", _, _) = processStat x "STAT"

processCommit :: GitState -> State -> Line -> GitState
processCommit a@(s, (_, author, date, fs), r) ns l =
  if null match then a else (ns, (match!!0!!1, author, date, fs), r)
  where match = l =~ "^commit (.+)" :: [[String]]

processAuthor :: GitState -> State -> Line -> GitState
processAuthor (s, (sha, _, date, fs), r) ns l = (ns, (sha, author, date, fs), r)
  where author = (l =~ "^Author: (.+)")!!0!!1

processDate :: GitState -> State -> Line -> GitState
processDate (s, (sha, author, _, fs), r) ns l = (ns, (sha, author, date, fs), r)
  where date = (l =~ "^Date: (.+)")!!0!!1

processStatBegin :: GitState -> State -> Line -> GitState
processStatBegin x ns l = if startsEmpty then x else processStat x ns l
  where startsEmpty = l =~ "^\\s" :: Bool

processStat :: GitState -> State -> Line -> GitState
processStat x ns l = if null match then flushState x else addStat x (match!!0)
  where match = l =~ "^([0-9]+)\\s+([0-9]+)\\s+(.+)" :: [[String]] -- \\d instead of [0-9] ain't working
        addStat (_, (sha, author, date, fs), r) (_:a:d:p:_) =
          (ns, (sha, author, date, (toStat a d p):fs), r)


