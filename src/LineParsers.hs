{-# LANGUAGE OverloadedStrings #-}

module LineParsers (
  ParserDef,
  ParserDefs,
  loadParsers,
  isEmptyLine,
  isSingleLineComment,
  isMultiLineCommentStart,
  isMultiLineCommentEnd,
  getLang
) where

import Text.Regex.Posix
import Data.Map as Map
import Data.Maybe
import Data.List as List
import Control.Applicative

import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), (.:), (.:?))
import qualified Data.ByteString.Char8 as BS

type RegExp = String

data ParserDef = ParserDef { lang :: String
                           , singleLine :: Maybe RegExp
                           , multiLineStart :: Maybe RegExp
                           , multiLineEnd :: Maybe RegExp
                           } deriving (Show)

instance FromJSON ParserDef where
  parseJSON (Y.Object v) =
    ParserDef <$>
      v .: "lang" <*>
      v .:? "singleLine" <*>
      v .:? "multiLineStart" <*>
      v .:? "multiLineEnd"

type ParserDefs = Map String ParserDef

loadParsers :: IO ParserDefs
loadParsers = do
  conf <- BS.readFile "./src/lineParsers.yaml"
  let defs = fromMaybe [] $ (Y.decode conf :: Maybe [ParserDef])
  return $ keyBy lang defs

keyBy :: (Ord b) => (a -> b) -> [a] -> Map b a
keyBy toKey = Map.fromList . (List.map toTuple)
  where toTuple x = (toKey x, x)

-- this would typically also read from a config file, hence the IO monad
getParsers :: IO ParserDefs
getParsers = return $ Map.fromList $ List.map (\p -> (lang p, p)) parsers
  where parsers = [ ParserDef { lang="js"
                              , singleLine=Just "^\\s*\\/\\/"
                              , multiLineStart=Just "^\\s*\\/\\*"
                              , multiLineEnd=Just "\\*\\/"
                              }
                  , ParserDef { lang="json"
                              , singleLine=Nothing
                              , multiLineStart=Nothing
                              , multiLineEnd=Nothing
                              }
                  , ParserDef { lang="hs"
                              , singleLine=Just "^\\s*--"
                              , multiLineStart=Just "^\\s*\\{-"
                              , multiLineEnd=Just "-}"
                              }
                  , ParserDef { lang="yaml"
                              , singleLine=Just "^\\s*#"
                              , multiLineStart=Nothing
                              , multiLineEnd=Nothing
                              }
                  , ParserDef { lang="md"
                              , singleLine=Nothing
                              , multiLineStart=Just "^\\s*<!--"
                              , multiLineEnd=Just "-->"
                              }
                  ]

getLang :: ParserDef -> String
getLang = lang

isEmptyLine :: String -> Bool
isEmptyLine x = x =~ ("^\\s*$" :: String)

isSingleLineComment :: ParserDef -> String -> Bool
isSingleLineComment parser = matchMaybe $ singleLine parser

isMultiLineCommentStart :: ParserDef -> String -> Bool
isMultiLineCommentStart parser = matchMaybe $ multiLineStart parser

isMultiLineCommentEnd :: ParserDef -> String -> Bool
isMultiLineCommentEnd parser = matchMaybe $ multiLineEnd parser

matchMaybe :: Maybe RegExp -> String -> Bool
matchMaybe Nothing _ = False
matchMaybe (Just regexp) line = line =~ regexp
