module Util
( values
) where

import Data.Map as Map
import Data.List as List

values :: Map k v -> [v]
values = (List.map snd) . Map.toList

