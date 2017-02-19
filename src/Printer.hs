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
, padLeft
, padRight
, padLeftS
, padRightS
, toRow
, toRowF
, toPanel
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

padLeftS = padLeft " "
padRightS = padRight " "

toLine :: String -> Int -> String
toLine c w = concatMap (\ _ -> c) [0..w]

toRow :: [Int] -> [String] -> String
toRow x y = concatMap spaceOut tuple
  where spaceOut (w, c) = padRight " " w c
        tuple = zip x y

toRowF :: [String -> String] -> [String] -> String
toRowF fs r = concat $ toRowFRaw fs r

toRowFRaw :: [String -> String] -> [String] -> [String]
toRowFRaw x y = map apply $ zip x y
  where apply (f, i) = f i

toPanel :: [Int] -> [[String]] -> [String]
toPanel dim (header:rows) = concat [intro, outro]
  where intro = [toPanelHeader dim header, line (sum dim)]
        outro = map (toPanelRow dim) rows

toPanelHeader :: [Int] -> [String] -> String
toPanelHeader dim header = concat . firstBold $ toPanelRowRaw dim header
  where firstBold (x:xs) = bold x:xs

toPanelRow :: [Int] -> [String] -> String
toPanelRow d r = concat $ toPanelRowRaw d r

toPanelRowRaw :: [Int] -> [String] -> [String]
toPanelRowRaw dim row = toRowFRaw (applyDim dim fs) row
  where fs = getPanelRowFormatter . length $ row

getPanelRowFormatter :: Int -> [Int -> String -> String]
getPanelRowFormatter i = padRightS:map (\_ -> padLeftS) [0..(i - 1)]

applyDim :: [a] -> [a -> b -> b] -> [b -> b]
applyDim a b = map apply $ zip b a
  where apply (f, d) = f d






