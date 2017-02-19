{-# LANGUAGE FlexibleContexts #-}

module Hot
( printStats
) where

import Data.Maybe
import Data.Map as Map
import System.Process
import System.IO
import Text.Regex.Posix

import FileStat
import Contributor
import Commit

import Printer as P
import Util (values)


type GitLineData = (String, String, String, [FileStat])
type State = String
type GitState = (State, GitLineData, [Commit])
type Line = String

emptyLineData = ("", "", "", [])

gitLog :: String -> IO String
gitLog timeframe = readProcess cmd args []
  where cmd = "git"
        args = ["log", "--numstat", "--no-color", "--since=" ++ timeframe]

printStats :: String -> IO ()
printStats timeframe = do
  gitOutput <- gitLog timeframe
  let commits = parseGitOutput gitOutput
  let contributors = collectContributors commits
  print $ getContributorStats (contributors!!0)

parseGitOutput :: String -> [Commit]
parseGitOutput = reverse . takeResult . processLines . lines
  where takeResult (_, _, x) = x

flushState :: GitState -> GitState
flushState all@(s, d@(sha, _, _, _), res) =
  if Prelude.null sha then all else ("BEGIN", emptyLineData, addRecord d)
   where addRecord (s, a, d, f) = (createCommit s a d f):res

processLines :: [Line] -> GitState
processLines = Prelude.foldl processLine ("BEGIN", emptyLineData, [])

processLine :: GitState -> Line -> GitState
processLine x@("BEGIN", _, _) = processCommit x "AUTHOR"
processLine x@("AUTHOR", _, _) = processAuthor x "DATE"
processLine x@("DATE", _, _) = processDate x "STAT_BEGIN"
processLine x@("STAT_BEGIN", _, _) = processStatBegin x "STAT"
processLine x@("STAT", _, _) = processStat x "STAT"

processCommit :: GitState -> State -> Line -> GitState
processCommit a@(s, (_, author, date, fs), r) ns l =
  if Prelude.null match then a else (ns, (match!!0!!1, author, date, fs), r)
  where match = l =~ "^commit (.+)" :: [[String]]

processAuthor :: GitState -> State -> Line -> GitState
processAuthor a@(s, (sha, _, date, fs), r) ns l =
  -- this line can indicate a merge commit - we ignore these!
  if Prelude.null match then a else (ns, (sha, match!!0!!1, date, fs), r)
  where match = (l =~ "^Author:.+<(.+)>")

processDate :: GitState -> State -> Line -> GitState
processDate (s, (sha, author, _, fs), r) ns l = (ns, (sha, author, date, fs), r)
  where date = (l =~ "^Date:   (.+)")!!0!!1

processStatBegin :: GitState -> State -> Line -> GitState
processStatBegin x ns l = if startsEmpty then x else processStat x ns l
  where startsEmpty = Prelude.null l || l =~ "^\\s+" :: Bool

processStat :: GitState -> State -> Line -> GitState
processStat x ns l = if Prelude.null match then flushState x else addStat x (match!!0)
  where match = l =~ "^([0-9]+)\\s+([0-9]+)\\s+(.+)" :: [[String]] -- \\d instead of [0-9] ain't working
        addStat (_, (sha, author, date, fs), r) (_:a:d:p:_) =
          (ns, (sha, author, date, (createFileStat a d p):fs), r)


collectContributors :: [Commit] -> [Contributor]
collectContributors = values . (Prelude.foldr collectContribFromCommit Map.empty)

collectContribFromCommit :: Commit -> Map String Contributor -> Map String Contributor
collectContribFromCommit com res = Map.insert author nextContrib res
  where author = getCommitAuthor com
        nextContrib = addCommitToContributor com $ getContributor author res

getContributor :: String -> Map String Contributor -> Contributor
getContributor author cs = fromMaybe (createContributor author) (Map.lookup author cs)

