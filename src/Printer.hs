module Printer
( nl
, join
, style
, bold
, inYellow
, inGreen
, inRed
, line
, line80
, dline
, dline80
, toRow
) where

import Data.List (intercalate)

nl :: String -> String
nl str = str ++ "\n"

join :: [String] -> String
join = intercalate "\n"

style :: Int -> String -> String
style code text = "\ESC[" ++ (show code) ++ "m" ++ text ++ "\ESC[0m"

bold = style 1
inYellow = style 93
inGreen = style 92
inRed = style 91

line :: Int -> String
line = toLine "-"

dline :: Int -> String
dline = toLine "="

line80 = line 80
dline80 = dline 80

padLeft :: String -> Int -> String -> String
padLeft padder width str = (toLine padder rest) ++ str
  where rest = width - length str

padRight :: String -> Int -> String -> String
padRight padder width str = str ++ (toLine padder rest)
  where rest = width - length str

toLine :: String -> Int -> String
toLine c w = concatMap (\ _ -> c) [0..w]

toRow :: [Int] -> [String] -> String
toRow x y = concatMap spaceOut tuple
  where spaceOut (w, c) = padRight " " w c
        tuple = zip x y



