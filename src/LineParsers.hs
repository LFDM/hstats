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

data ParserDef = ParserDef { lang :: String
                           , singleLine :: String
                           , multiLineStart :: String
                           , multiLineEnd :: String
                           } deriving (Show)

type ParserDefs = Map String ParserDef


getParsers :: IO ParserDefs
getParsers = return $ Map.fromList $ List.map (\p -> (lang p, p)) parsers
  where parsers = [ ParserDef { lang="js"
                              , singleLine="^\\s\\/\\/"
                              , multiLineStart="\\/\\*"
                              , multiLineEnd="\\*\\/"
                              }
                  ]

getLang :: ParserDef -> String
getLang = lang

isEmptyLine :: String -> Bool
isEmptyLine x = x =~ "^\\s*$"

isSingleLineComment :: ParserDef -> String -> Bool
isSingleLineComment parser line = line =~ (singleLine parser)

isMultiLineCommentStart :: ParserDef -> String -> Bool
isMultiLineCommentStart parser line = line =~ (multiLineStart parser)

isMultiLineCommentEnd :: ParserDef -> String -> Bool
isMultiLineCommentEnd parser line = line =~ (multiLineEnd parser)


