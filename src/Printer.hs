module Printer
( nl
, colorize
, inYellow
, inGreen
, inRed
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
