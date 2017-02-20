module Util
( values
, removeUnicode
, lookupWithDefault
) where

import Data.Maybe
import Data.Map as Map
import Data.List as List

values :: Map k v -> [v]
values = (List.map snd) . Map.toList

lookupWithDefault :: (Ord k) => v -> k -> Map k v -> v
lookupWithDefault d key m = fromMaybe d (Map.lookup key m)

removeUnicode :: String -> String
removeUnicode xs = [ x | x <- xs, x `notElem` "\246" ]


