module Loc
( printLineCountsAtPath
) where

import System.Environment
import System.IO
import System.Directory
import Control.Monad
import Data.List as List
import Data.Map as Map
import Data.Maybe
import Data.Time
import qualified Data.ByteString.Char8 as BS

import Util (values)
import Printer as P
import LineParsers ( ParserDef
                   , ParserDefs
                   , loadParsers
                   , findParser
                   , isEmptyLine
                   , isSingleLineComment
                   , isMultiLineCommentStart
                   , isMultiLineCommentEnd
                   , getName
                   , getExt
                   )

type TempCounter = (Int, Int, Int, Bool)
type Counter = (String, Int, Int, Int, Int)
type CounterMap = Map String Counter

header = ["Langugage", "files", "code", "comment", "blank"]
dimensions = [20, 12, 12, 12, 12]
ignoredKeyword = "ignored"

printLineCountsAtPath :: String -> IO ()
printLineCountsAtPath path = do
  parsers <- loadParsers
  counts <- countAtPaths parsers Map.empty [path]

  let ignored = getIgnoredFilesCount counts
  let withoutIgnored = Map.delete ignoredKeyword counts
  start <- getCurrentTime
  putStrLn $ renderCounts withoutIgnored
  stop <- getCurrentTime
  putStrLn $ renderStatus ignored (diffUTCTime stop start)
  return ()

getIgnoredFilesCount :: CounterMap -> Int
getIgnoredFilesCount c = maybe 0 extractCount ignored
  where ignored = Map.lookup ignoredKeyword c
        extractCount (_, count, _, _, _) = count

renderStatus :: Int -> NominalDiffTime -> String
renderStatus iF tE =  unwords [ show iF
                              , "file(s) ignored, took"
                              , show tE
                              ]


renderCounts :: CounterMap -> String
renderCounts cm = P.join [ P.line80
                         , P.bold $ renderRow header
                         , P.line80
                         , rows
                         , P.line80
                         , (renderRow . counterToStrs ) total
                         , P.line80
                         ]
  where total = Map.foldr mergeCounter ("TOTAL", 0, 0, 0, 0) cm
        rows = (P.join. (List.map (renderRow . counterToStrs)) . values ) cm


renderRow :: [String] -> String
renderRow = P.toRow dimensions

counterToStrs :: Counter -> [String]
counterToStrs (lang, f, c, co, b) = [lang, show f, show c, show co, show b]





countAtPaths :: ParserDefs -> CounterMap -> [FilePath] -> IO CounterMap
countAtPaths parsers = foldM (countAtPath parsers)

countAtPath :: ParserDefs -> CounterMap -> FilePath -> IO CounterMap
countAtPath parsers res path = do
  isFile <- doesFileExist path
  if isFile
    then countInFile parsers res path
    else getFilePathsInDir path >>= countAtPaths parsers res

countInFile :: ParserDefs -> CounterMap -> FilePath -> IO CounterMap
countInFile parsers res path = do
  content <- BS.readFile path
  return $ addCounter res $ parseFile parser content
  where parser = findParser parsers path

addCounter :: CounterMap -> Counter -> CounterMap
addCounter res counter@(lang, _, _, _, _) = Map.insert lang merged res
  where merged = maybeMergeCounter counter $ Map.lookup lang res

maybeMergeCounter :: Counter -> Maybe Counter -> Counter
maybeMergeCounter counter (Just other) = mergeCounter counter other
maybeMergeCounter counter Nothing = counter

mergeCounter :: Counter -> Counter -> Counter
mergeCounter (_, f1, c1, co1, b1) (lang, f2, c2, co2, b2) =
  (lang, f1 + f2, c1 + c2, co1 + co2, b1 + b2)

parseFile :: Maybe ParserDef -> BS.ByteString -> Counter
parseFile Nothing _ = (ignoredKeyword, 1, 0, 0, 0)
parseFile (Just parser) f = toCounter . parseLines . BS.lines $ f
  where toCounter = (tempToCounter . getName) parser
        parseLines = List.foldr (parseLine parser) (0, 0, 0, False)

tempToCounter :: String -> TempCounter -> Counter
tempToCounter name (code, comment, blank, _) = (name, 1, code, comment, blank)

-- need to recheck if multiline commentis can be nested - if they are
-- we need to count comment openings instead of just flipping a boolean
parseLine :: ParserDef -> BS.ByteString -> TempCounter -> TempCounter
parseLine parser line (code, comment, blank, withinComment)
  -- arguable, empty lines in comments could count as comment too
  | empty = (code, comment, blank + 1, withinComment)
  | single = (code, comment + 1, blank, withinComment)
  | multiStart = (code, comment + 1, blank, not multiEnd)
  | multiEnd = (code, comment + 1, blank, False)
  | otherwise = if withinComment
                  then (code, comment + 1, blank, withinComment)
                  else (code + 1, comment, blank, withinComment)
  where empty = isEmptyLine line
        single = isSingleLineComment parser line
        multiStart = isMultiLineCommentStart parser line
        multiEnd = isMultiLineCommentEnd parser line

getFilePathsInDir :: FilePath -> IO [FilePath]
getFilePathsInDir path = getDirectoryContents path
  >>= filterM (return . not . isSpecialPath)
  >>= mapM (return . toAbsolutePath path)
  where toAbsolutePath base other = base ++ "/" ++ other
        isSpecialPath name = name == ".." || name == "."


