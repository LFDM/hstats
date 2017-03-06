{-# LANGUAGE OverloadedStrings #-}

module CommitMsgParsers
( loadParsers
, findCategory
, unknownCategory
, ParserDef
, getParserName
) where


import Text.Regex.Posix
import Data.Map as Map
import Data.List as List
import Data.Maybe
import Control.Applicative

import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), (.:), (.:?))
import qualified Data.ByteString.Char8 as BS

import Paths_hstats

type RegExp = String

data ParserDef = ParserDef { name :: String
                           , matcher :: RegExp
                           } deriving (Show)

instance FromJSON ParserDef where
  parseJSON (Y.Object v) =
    ParserDef <$>
      v .: "name" <*>
      v .: "matcher"

unknownCategory = "unknown"

loadParsers :: IO [ParserDef]
loadParsers = do
  conf <- getDataFileName "data/commitMsgPrefixes.yaml" >>= BS.readFile
  return $ fromMaybe [] $ (Y.decode conf :: Maybe [ParserDef])

findCategory :: [ParserDef] -> String -> String
findCategory defs msg = maybe unknownCategory name . List.find match $ defs
  where match p = msg =~ matcher p :: Bool

getParserName :: ParserDef -> String
getParserName = name

