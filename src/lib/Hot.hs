{-# LANGUAGE FlexibleContexts #-}

module Hot
( printStats
) where

import Data.Function
import Data.Maybe
import Data.Map as Map
import Data.List as List
import Data.Time
import System.FilePath (combine, joinPath, splitDirectories)
import System.Process
import System.IO
import Text.Regex.Posix

import FileStat
import Contributor
import Commit
import GitFile
import GitDir
import Categories

import Printer as P
import Util


type GitLineData = (String, String, String, String, [FileStat])
type State = String
type GitState = (State, GitLineData, [Commit])
type Line = String

filePanelPathLen = 64

emptyLineData = ("", "", "", "", [])

gitLog :: String -> String -> String -> IO String
gitLog timeframe dir accuracy = readProcess cmd (concat [args, path]) []
  where cmd = "git"
        path = if Prelude.null dir then [] else ["--", dir]
        args = [ "log"
               , "--numstat"
               , "-M" ++ accuracy
               , "-C" ++ accuracy
               , "--no-color"
               , "--since=" ++ timeframe
               ] ++ path

gitRoot :: IO String
gitRoot = readProcess cmd args []
  where cmd = "git"
        args = [ "rev-parse", "--show-prefix"]


printStats :: String -> String -> Int -> String -> IO ()
printStats timeframe dir depth accuracy = do
  start <- getCurrentTime
  gitOutput <- gitLog timeframe dir accuracy
  rootDir <- gitRoot
  let fullDir = combine (trimR rootDir) dir
  putStrLn $ unwords ["Analyzing commits in", fullDir]
  putStrLn ""

  let commits = parseGitOutput gitOutput
  let contributors = collectContributors commits
  putStrLn $ P.join . toCommitterPanel . toComPanelArgs $ contributors
  putStrLn ""

  let files = collectFiles commits
  putStrLn $ P.join . toFilesPanel . toFPanelArgs fullDir $ files
  putStrLn ""

  let dir = collectDirs fullDir files
  putStrLn $ P.join . toDirPanel . toDirPanelArgs depth fullDir $ dir
  putStrLn ""

  categories <- collectCategories commits
  putStrLn $ P.join . toCtgPanel . toCtgPanelArgs $ categories
  putStrLn ""

  stop <- getCurrentTime
  putStrLn $ "Took " ++ show (diffUTCTime stop start)

  return ()


toCommitterPanel :: [[String]] -> [String]
toCommitterPanel rows = P.toPanel dimensions (header:rows)
  where dimensions = [filePanelPathLen, 6, 6, 8, 8, 8, 6, 6, 6]
        header = ["Top Committers", "Com", "Files", "+", "-", "+/-", "~+", "~-", "~+/-"]

toComPanelArgs :: [Contributor] -> [[String]]
toComPanelArgs = List.map contributorToStatLine . take 15 . sortContribsByCommits

toFilesPanel :: [[String]] -> [String]
toFilesPanel rows = P.toPanel dimensions (header:rows)
  where dimensions = [64, 6, 6, 8, 8, 8, 6, 6, 6]
        header = ["Top Files", "Com", "Auth", "+", "-", "+/-", "~+", "~-", "~+/-"]

toFPanelArgs :: String -> [GitFile]-> [[String]]
toFPanelArgs dir = List.map toStatLine . take 15 . sortGitFilesByCommits
  where toStatLine = shortenFN . gitFileToStatLine
        shortenFN (x:xs)= shortenFileName filePanelPathLen dir x:xs

toDirPanel :: [[String]] -> [String]
toDirPanel rows = P.toPanel dimensions (header:rows)
  where dimensions = [64, 6, 6, 8, 8, 8]
        header = ["Top Dirs", "Com", "Auth", "+", "-", "+/-"]

toCtgPanel :: [[String]] -> [String]
toCtgPanel rows = P.toPanel dimensions (header:rows)
  where dimensions = [64, 6]
        header = ["Top Categories", "Com"]

toCtgPanelArgs :: [Category]-> [[String]]
toCtgPanelArgs = List.map categoryToStatLine . take 15 . sortCategoriesByCommits

toDirPanelArgs :: Int -> String -> GitDir -> [[String]]
toDirPanelArgs depth rootDir dir = zipWith merge paths stats
  where stats = List.map gitDirToStatLine $ gitDirToNormalizedSortedList depth dir
        paths = renderAsTree . (gitDirToSortedPathTree depth "") $ dir
        merge p (_:xs) = p:xs

parseGitOutput :: String -> [Commit]
parseGitOutput = reverse . takeResult . processLines . lines
  where takeResult (_, _, x) = x

flushState :: GitState -> GitState
flushState all@(s, d@(sha, _, _, _, _), res) =
  if Prelude.null sha then all else ("BEGIN", emptyLineData, addRecord d)
   where addRecord (s, a, d, m, f) = (createCommit s a d m f):res

processLines :: [Line] -> GitState
processLines = Prelude.foldr processLine ("BEGIN", emptyLineData, [])

processLine :: Line -> GitState -> GitState
processLine l x@("BEGIN", _, _) = processCommit x "AUTHOR" l
processLine l x@("AUTHOR", _, _) = processAuthor x "MSG" l
processLine l x@("MSG", _, _) = processMsg x "DATE" l
processLine l x@("DATE", _, _) = processDate x "STAT_BEGIN" l
processLine l x@("STAT_BEGIN", _, _) = processStatBegin x "STAT" l
processLine l x@("STAT", _, _) = processStat x "STAT" l

processCommit :: GitState -> State -> Line -> GitState
processCommit a@(s, (_, author, date, msg, fs), r) ns l =
  if Prelude.null match then a else (ns, (match!!0!!1, author, date, msg, fs), r)
  where match = l =~ "^commit (.+)" :: [[String]]

processAuthor :: GitState -> State -> Line -> GitState
processAuthor a@(s, (sha, _, date, msg, fs), r) ns l =
  -- this line can indicate a merge commit - we ignore these!
  if Prelude.null match then a else (ns, (sha, match!!0!!1, date, msg, fs), r)
  where match = (removeUnicode l) =~ "^Author:.*<(.+)>"

processDate :: GitState -> State -> Line -> GitState
processDate (s, (sha, author, _, msg, fs), r) ns l = (ns, (sha, author, date, msg, fs), r)
  where date = (l =~ "^Date:   (.+)")!!0!!1

processMsg :: GitState -> State -> Line -> GitState
processMsg x@(_, (sha, a, d, _, fs), r) ns l =
  if Prelude.null match then x else (ns, (sha, a, d, trim msg, fs), r)
  where msg = match!!0!!1
        match = l =~ "^\\s+(.+)$"

processStatBegin :: GitState -> State -> Line -> GitState
processStatBegin x ns l = if startsEmpty then x else processStat x ns l
  where startsEmpty = Prelude.null l || l =~ "^\\s+" :: Bool

processStat :: GitState -> State -> Line -> GitState
processStat x ns l = if Prelude.null match then flushState x else addStat x (match!!0)
  where match = l =~ "^([0-9]+)\\s+([0-9]+)\\s+(.+)" :: [[String]] -- \\d instead of [0-9] ain't working
        addStat (_, (sha, author, date, m, fs), r) (_:a:d:p:_) =
          (ns, (sha, author, date, m, (createFileStat a d p):fs), r)

-------------------

collectContributors :: [Commit] -> [Contributor]
collectContributors = values . (Prelude.foldr collectContribFromCommit Map.empty)

collectContribFromCommit :: Commit -> Map String Contributor -> Map String Contributor
collectContribFromCommit com res = Map.insert author nextContrib res
  where author = getCommitAuthor com
        nextContrib = addCommitToContributor com $ getContributor author res

getContributor :: String -> Map String Contributor -> Contributor
getContributor author = lookupWithDefault (createContributor author) author

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

------------------


