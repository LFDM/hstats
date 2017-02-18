module Loc
( printLineCountsAtPath
) where

import System.Environment
import System.IO
import System.Directory
import Control.Monad
import Data.List as List
import Data.Map as Map

import System.FilePath (takeExtension)

import LineParsers ( ParserDef
                   , ParserDefs
                   , getParsers
                   , isEmptyLine
                   , isSingleLineComment
                   , isMultiLineCommentStart
                   , isMultiLineCommentEnd
                   , getLang
                   )

type TempCounter = (Int, Int, Int, Bool)
type Counter = (String, Int, Int, Int, Int)

type CounterMap = Map String Counter

printLineCountsAtPath :: String -> IO ()
printLineCountsAtPath path = do
  parsers <- loadParsers
  counts <- countAtPaths parsers Map.empty [path]
  print $ values counts
  return ()

-- allow paths to files directly, check first if file exists

values :: Map k v -> [v]
values = (List.map snd) . Map.toList

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
  content <- readFile path
  return $ addCounter res $ parseFile parser content
  where parser = Map.lookup ((extToLang . takeExtension) path) parsers
        extToLang "" = ""
        extToLang (_:xs) = xs -- remove the .

addCounter :: CounterMap -> Counter -> CounterMap
addCounter res counter@(lang, _, _, _, _) = Map.insert lang merged res
  where merged = mergeCounter counter $ Map.lookup lang res

mergeCounter :: Counter -> Maybe Counter -> Counter
mergeCounter (lang, f1, c1, co1, b1) (Just (_, f2, c2, co2, b2)) = (lang, f1 + f2, c1 + c2, co1 + co2, b1 + b2)
mergeCounter counter Nothing = counter


parseFile :: Maybe ParserDef -> String -> Counter
parseFile Nothing _ = ("ignored", 1, 0, 0, 0)
parseFile (Just parser) f = toCounter . parseLines . lines $ f
  where toCounter = (tempToCounter . getLang) parser
        parseLines = List.foldr (parseLine parser) (0, 0, 0, False)

tempToCounter :: String -> TempCounter -> Counter
tempToCounter lang (code, comment, blank, _) = (lang, 1, code, comment, blank)


-- need to recheck if multiline comments can be nested - if they are
-- we need to count comment openings instead of just flipping a boolean
parseLine :: ParserDef -> String -> TempCounter -> TempCounter
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


