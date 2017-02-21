{-# LANGUAGE FlexibleContexts #-}

module Util
( values
, removeUnicode
, lookupWithDefault
, shorten
, shortenWithEllipsis
, addUnique
, mergeUnique
, normalizePath
, removeLeading
, removeLeadingSlash
, trimL
, trimR
, trim
) where

import Data.Char
import Data.Maybe
import Data.Map as Map
import Data.List as List
import System.FilePath
import Text.Regex.Posix

values :: Map k v -> [v]
values = (List.map snd) . Map.toList

lookupWithDefault :: (Ord k) => v -> k -> Map k v -> v
lookupWithDefault d key m = fromMaybe d (Map.lookup key m)

removeUnicode :: String -> String
removeUnicode xs = [ x | x <- xs, x `notElem` "\246" ]

shorten :: Int -> String -> String
shorten limit s
  | l > limit = drop (l - limit) s
  | otherwise = s
  where l = length s

shortenWithEllipsis :: Int -> String -> String
shortenWithEllipsis limit s
  | l > limit = "..." ++ (drop 3 . shorten limit) s
  | otherwise = s
  where l = length s

slice :: Int -> Int -> String -> String
slice start end = take (end - start + 1) . drop start

addUnique :: (Eq a) => a -> [a] -> [a]
addUnique x xs = if x `elem` xs then xs else x:xs

mergeUnique :: (Eq a) => [a] -> [a] -> [a]
mergeUnique = Prelude.foldr addUnique

normalizePath :: FilePath -> FilePath
normalizePath path = joinPath $ List.foldr norm [] paths
  where paths = splitDirectories path
        norm p ps = if p =~ "^\\.+$" then drop (length p) ps else p:ps

removeLeadingSlash = removeLeading "/"

removeLeading :: [a] -> [a] -> [a]
removeLeading x = drop $ (length x)

trimR :: String -> String
trimR = reverse . dropWhile isSpace . reverse

trimL :: String -> String
trimL = dropWhile isSpace

trim :: String -> String
trim = trimR . trimL


