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
, replaceLeading
, trimL
, trimR
, trim
, sortByAccessorDesc
, shortenFileName
, renderAsTree
, StringTree(STN)
) where

import Data.Char
import Data.Function
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

removeLeading :: (Eq a) => [a] -> [a] -> [a]
removeLeading x ys = if startsWith x ys then drop (length x) ys else ys

startsWith :: (Eq a) => [a] -> [a] -> Bool
startsWith x y = x == take (length x) y

replaceLeading :: (Eq a) => [a] -> [a] -> [a] -> [a]
replaceLeading leading replace str = if startsWith leading str then repl else str
  where repl = r ++ drop l str
        r = take l $ concat (replicate l replace)
        l = length leading


trimR :: String -> String
trimR = reverse . dropWhile isSpace . reverse

trimL :: String -> String
trimL = dropWhile isSpace

trim :: String -> String
trim = trimR . trimL

shortenFileName :: Int -> String -> String -> String
shortenFileName maxLen rootDir n = shortenWithEllipsis maxLen (dropPrefix n)
  where dropPrefix = removeLeadingSlash . removeLeading (normalizePath rootDir)

sortByAccessorDesc :: (Ord b) => (a -> b) -> [a] -> [a]
sortByAccessorDesc f = List.sortBy (check `on` f)
  where check = flip compare


data StringTree = STN (String, [StringTree]) deriving (Show)

instance Eq StringTree where
  x == y = isEqualStringTree x y

isEqualStringTree :: StringTree -> StringTree -> Bool
isEqualStringTree (STN (p1, cs1)) (STN (p2, cs2)) = p1 == p2 && cs1 == cs2

renderAsTree :: StringTree -> [String]
renderAsTree = renderAsTree' id

renderAsTree' :: (String -> String) -> StringTree -> [String]
renderAsTree' f = normalizePipes . renderAsTreeRec f 0 True

renderAsTreeRec :: (String -> String) -> Int -> Bool -> StringTree -> [String]
renderAsTreeRec f level isLast (STN (s, cs)) = current:nextLevel
  where prefix 0 = ""
        prefix 1 = unwords [sign, ""]
        prefix l = unwords $ concat [(replicate (l - 1) "│"), [sign, ""]]
        sign = if isLast then "└" else "├"
        current = (prefix level) ++ f s
        nextLevel = concatMap renderNext cs
        renderNext c = renderAsTreeRec f (level + 1) (c == (last cs)) c

normalizePipes :: [String] -> [String]
normalizePipes [] = []
normalizePipes (x:[]) = [x]
normalizePipes (x:xs) = x:normalizePipes (next:tail xs)
  where next = normalizePipes' (head xs) x

normalizePipes' :: String -> String -> String
normalizePipes' a b = List.foldr tryReplace a is
  where is = elemIndices '│' a
        tryReplace i x = if isReplaceable i b then replace i " " x else x
        isReplaceable i x = i < (length x) && ((x!!i) == ' ' || (x!!i) == '└')

replace :: Int -> [a] -> [a] -> [a]
replace i x xs
  | i < (length xs) = (take i xs) ++ x ++ (drop (i + 1) xs)
  | otherwise = xs
