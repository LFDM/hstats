module Util
( values
, removeUnicode
) where

import Data.Map as Map
import Data.List as List

values :: Map k v -> [v]
values = (List.map snd) . Map.toList

removeUnicode :: String -> String
removeUnicode xs = [ x | x <- xs, x `notElem` "\246" ]

