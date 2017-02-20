{-# LANGUAGE FlexibleContexts #-}

module Hot
( printStats
) where

import Data.Function
import Data.Maybe
import Data.Map as Map
import Data.List as List
import Data.Time
import System.Process
import System.IO
import Text.Regex.Posix

import FileStat
import Contributor
import Commit
import GitFile

import Printer as P
import Util (lookupWithDefault, removeUnicode, values)


type GitLineData = (String, String, String, [FileStat])
type State = String
type GitState = (State, GitLineData, [Commit])
type Line = String

filePanelPathLen = 44

emptyLineData = ("", "", "", [])

gitLog :: String -> IO String
gitLog timeframe = readProcess cmd args []
  where cmd = "git"
        args = ["log", "--numstat", "--no-color", "--since=" ++ timeframe]

printStats :: String -> IO ()
printStats timeframe = do
  start <- getCurrentTime
  gitOutput <- gitLog timeframe
  let commits = parseGitOutput gitOutput
  let contributors = collectContributors commits
  putStrLn $ P.join . toCommitterPanel . toComPanelArgs $ contributors
  putStrLn ""

  let files = collectFiles commits
  putStrLn $ P.join . toFilesPanel . toFPanelArgs $ files
  putStrLn ""

  stop <- getCurrentTime
  putStrLn $ "Took " ++ show (diffUTCTime stop start)
  return ()

  where toComPanelArgs = List.map contributorToStatLine . take 10 . sortContribsByCommits
        toFPanelArgs = List.map (gitFileToStatLineShort filePanelPathLen) . take 15 . sortGitFilesByCommits


toCommitterPanel :: [[String]] -> [String]
toCommitterPanel rows = P.toPanel dimensions (header:rows)
  where dimensions = [filePanelPathLen, 6, 6, 8, 8, 8]
        header = ["Top Committers", "Com", "Files", "+", "-", "+/-"]

toFilesPanel :: [[String]] -> [String]
toFilesPanel rows = P.toPanel dimensions (header:rows)
  where dimensions = [44, 6, 6, 8, 8, 8]
        header = ["Top Files", "Com", "Auth", "+", "-", "+/-"]

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
  where match = (removeUnicode l) =~ "^Author:.*<(.+)>"

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

-------------------

collectContributors :: [Commit] -> [Contributor]
collectContributors = values . (Prelude.foldr collectContribFromCommit Map.empty)

collectContribFromCommit :: Commit -> Map String Contributor -> Map String Contributor
collectContribFromCommit com res = Map.insert author nextContrib res
  where author = getCommitAuthor com
        nextContrib = addCommitToContributor com $ getContributor author res

getContributor :: String -> Map String Contributor -> Contributor
getContributor author = lookupWithDefault (createContributor author) author

sortContribsByCommits :: [Contributor] -> [Contributor]
sortContribsByCommits = List.sortBy (check `on` (commitCount . getContributorStats))
  where commitCount (c, _, _, _) = c
        check = flip compare

-------------------

collectFiles :: [Commit] -> [GitFile]
collectFiles = values . (Prelude.foldr collectFilesFromCommit Map.empty)

collectFilesFromCommit :: Commit -> Map String GitFile -> Map String GitFile
collectFilesFromCommit c m = Prelude.foldr (collectFileFromFileStats c) m commits
  where commits = getCommitFiles c

collectFileFromFileStats :: Commit -> FileStat -> Map String GitFile -> Map String GitFile
collectFileFromFileStats c fs m = Map.insert path ((nextGF . getFSChanges) fs) m
  where path = getFSPath fs
        nextGF (a, d) = addToGitFile c a d $ getGitFile path m

getGitFile :: String -> Map String GitFile -> GitFile
getGitFile p = lookupWithDefault (createGitFile p) p

sortGitFilesByCommits :: [GitFile] -> [GitFile]
sortGitFilesByCommits = List.sortBy (check `on` (length . getGitFileCommits))
  where commitCount (c, _, _, _) = c
        check = flip compare

------------------
