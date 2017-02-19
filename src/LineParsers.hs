{-# LANGUAGE OverloadedStrings #-}

module LineParsers (
  ParserDef,
  ParserDefs,
  loadParsers,
  findParser,
  isEmptyLine,
  isSingleLineComment,
  isMultiLineCommentStart,
  isMultiLineCommentEnd,
  getName,
  getExt
) where

import Text.Regex.Posix
import Data.Map as Map
import Data.Maybe
import Data.List as List
import Data.List.Split (splitOn)
import Control.Applicative
import System.FilePath (takeBaseName, takeExtension)

import Paths_hstats


import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), (.:), (.:?))
import qualified Data.ByteString.Char8 as BS

type RegExp = String

data ParserDef = ParserDef { ext :: String
                           , name :: String
                           , singleLine :: Maybe RegExp
                           , multiLineStart :: Maybe RegExp
                           , multiLineEnd :: Maybe RegExp
                           } deriving (Show)

instance FromJSON ParserDef where
  parseJSON (Y.Object v) =
    ParserDef <$>
      v .: "ext" <*>
      v .: "name" <*>
      v .:? "singleLine" <*>
      v .:? "multiLineStart" <*>
      v .:? "multiLineEnd"

type ParserDefs = Map String ParserDef

loadParsers :: IO ParserDefs
loadParsers = do
  conf <- getDataFileName "data/lineParsers.yaml" >>= BS.readFile
  let defs = fromMaybe [] $ (Y.decode conf :: Maybe [ParserDef])
  return $ keyBy ext defs

findParser :: ParserDefs -> FilePath -> Maybe ParserDef
findParser parsers path = findParserByExtensions parsers $ splitOn "." path


findParserByExtensions :: ParserDefs -> [String] -> Maybe ParserDef
findParserByExtensions parsers [] = Nothing
findParserByExtensions parsers (b:[]) = Nothing
findParserByExtensions parsers (b:exts) =
  if isNothing value then findParserByExtensions parsers exts else value
  where value = Map.lookup toExt parsers
        toExt = intercalate "." exts

keyBy :: (Ord b) => (a -> b) -> [a] -> Map b a
keyBy toKey = Map.fromList . (List.map toTuple)
  where toTuple x = (toKey x, x)

getName :: ParserDef -> String
getName = name

getExt :: ParserDef -> String
getExt = ext

isEmptyLine :: BS.ByteString -> Bool
isEmptyLine x = x =~ ("^\\s*$" :: BS.ByteString)

isSingleLineComment :: ParserDef -> BS.ByteString -> Bool
isSingleLineComment parser = matchMaybe $ singleLine parser

isMultiLineCommentStart :: ParserDef -> BS.ByteString -> Bool
isMultiLineCommentStart parser = matchMaybe $ multiLineStart parser

isMultiLineCommentEnd :: ParserDef -> BS.ByteString -> Bool
isMultiLineCommentEnd parser = matchMaybe $ multiLineEnd parser

matchMaybe :: Maybe RegExp -> BS.ByteString-> Bool
matchMaybe Nothing _ = False
matchMaybe (Just regexp) line = line =~ regexp
