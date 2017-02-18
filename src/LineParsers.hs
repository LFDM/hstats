module LineParsers (
  ParserDef,
  ParserDefs,
  getParsers,
  isEmptyLine,
  isSingleLineComment,
  isMultiLineCommentStart,
  isMultiLineCommentEnd,
  getLang
) where

import Text.Regex.Posix
import Data.Map as Map
import Data.List as List

type RegExp = String

data ParserDef = ParserDef { lang :: String
                           , singleLine :: Maybe RegExp
                           , multiLineStart :: Maybe RegExp
                           , multiLineEnd :: Maybe RegExp
                           } deriving (Show)

type ParserDefs = Map String ParserDef


-- this would typically also read from a config file, hence the IO monad
getParsers :: IO ParserDefs
getParsers = return $ Map.fromList $ List.map (\p -> (lang p, p)) parsers
  where parsers = [ ParserDef { lang="js"
                              , singleLine=Just "^\\s*\\/\\/"
                              , multiLineStart=Just "^\\s*\\/\\*"
                              , multiLineEnd=Just "\\*\\/"
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
isEmptyLine x = x =~ "^\\s*$"

isSingleLineComment :: ParserDef -> String -> Bool
isSingleLineComment parser = matchMaybe $ singleLine parser

isMultiLineCommentStart :: ParserDef -> String -> Bool
isMultiLineCommentStart parser = matchMaybe $ multiLineStart parser

isMultiLineCommentEnd :: ParserDef -> String -> Bool
isMultiLineCommentEnd parser = matchMaybe $ multiLineEnd parser

matchMaybe :: Maybe RegExp -> String -> Bool
matchMaybe Nothing _ = False
matchMaybe (Just regexp) line = line =~ regexp
