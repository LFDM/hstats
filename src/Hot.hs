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

import Printer as P
import Util


type GitLineData = (String, String, String, [FileStat])
type State = String
type GitState = (State, GitLineData, [Commit])
type Line = String

filePanelPathLen = 64

emptyLineData = ("", "", "", [])

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


printStats :: String -> String -> String -> IO ()
printStats timeframe dir accuracy = do
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

  let dir = collectDirs files
  putStrLn $ P.join . toDirPanel . toDirPanelArgs fullDir $ dir
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
        shortenFN (x:xs)= shortenFilename dir x:xs

toDirPanel :: [[String]] -> [String]
toDirPanel rows = P.toPanel dimensions (header:rows)
  where dimensions = [64, 6, 6, 8, 8, 8]
        header = ["Top Dirs", "Com", "Auth", "+", "-", "+/-"]

toDirPanelArgs :: String -> GitDir -> [[String]]
toDirPanelArgs rootDir dir = collect [] [dir]
  where collect acc [] = acc
        collect acc (x:xs) = collect (nextAcc acc x) xs
        nextAcc acc d = concat [acc, [toStat d], childrenToStat d]
        childrenToStat d = List.map toStat . sortGitDirsByCommits $ getGitDirChildren d
        toStat = shortenFN . gitDirToStatLine
        shortenFN (x:xs) = shortenFilename rootDir x:xs

parseGitOutput :: String -> [Commit]
parseGitOutput = reverse . takeResult . processLines . lines
  where takeResult (_, _, x) = x

flushState :: GitState -> GitState
flushState all@(s, d@(sha, _, _, _), res) =
  if Prelude.null sha then all else ("BEGIN", emptyLineData, addRecord d)
   where addRecord (s, a, d, f) = (createCommit s a d f):res

processLines :: [Line] -> GitState
processLines = Prelude.foldr processLine ("BEGIN", emptyLineData, [])

processLine :: Line -> GitState -> GitState
processLine l x@("BEGIN", _, _) = processCommit x "AUTHOR" l
processLine l x@("AUTHOR", _, _) = processAuthor x "DATE" l
processLine l x@("DATE", _, _) = processDate x "STAT_BEGIN" l
processLine l x@("STAT_BEGIN", _, _) = processStatBegin x "STAT" l
processLine l x@("STAT", _, _) = processStat x "STAT" l

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
sortContribsByCommits = sortByAccessorDesc $ commitCount . getContributorStats
  where commitCount (c, _, _, _) = c

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
sortGitFilesByCommits = sortByAccessorDesc $ length . getGitFileCommits

------------------

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
addParents (p, d) = addToParents' pPaths d
  where pPaths = "":splitDirectories p

addToParents' :: [String] -> GitDir -> Map String GitDir -> Map String GitDir
addToParents' [] _ ds = ds
addToParents' xs s ds = addToParents' pPaths s $ next ds
  where pPaths = init xs
        next = Map.insertWith mergeGitDir (concat pPaths) (gitSubDirToDir s)


sortGitDirsByCommits :: [GitDir] -> [GitDir]
sortGitDirsByCommits = sortByAccessorDesc $ length . getGitDirCommits

shortenFilename :: String -> String -> String
shortenFilename rootDir n = shortenWithEllipsis filePanelPathLen (dropPrefix n)
  where dropPrefix = removeLeadingSlash . removeLeading (normalizePath rootDir)

