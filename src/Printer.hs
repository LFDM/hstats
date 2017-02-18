module Printer
( nl
, colorize
, inYellow
, inGreen
, inRed
, line
, line80
, dline
, dline80
, toRow
) where

nl :: String -> String
nl str = str ++ "\n"

colorize :: Int -> String -> String
colorize code text = "\ESC[" ++ (show code) ++ "m" ++ text ++ "\ESC[0m"

inYellow :: String -> String
inYellow = colorize 93

inGreen :: String -> String
inGreen = colorize 92

inRed :: String -> String
inRed = colorize 91

line :: Int -> String
line = toLine "-"

line80 :: String
line80 = line 80

dline :: Int -> String
dline = toLine "="

dline80 :: String
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



