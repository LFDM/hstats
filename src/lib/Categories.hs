module Categories
( Category
, collectCategories
, sortCategoriesByCommits
, categoryToStatLine
) where

import Commit
import CommitMsgParsers
import Util

import Data.Map as Map
import Data.List as List

data Category = Category { name :: String
                         , commits :: [Commit]
                         }

collectCategories :: [Commit] -> IO [Category]
collectCategories commits = do
  defs <- loadParsers
  return $ toList . List.foldr (findAndAddCategory defs) (createCategoryMap defs) $ commits
    where toList = (List.map snd) . Map.toList

findAndAddCategory :: [ParserDef] -> Commit -> Map String Category -> Map String Category
findAndAddCategory defs c cs = add (Map.lookup category cs)
  where category = findCategory defs (getCommitMessage c)
        add Nothing = cs
        add (Just ctg) = Map.insert category (addCommitToCategory c ctg) cs

createCategoryMap :: [ParserDef] -> Map String Category
createCategoryMap defs = withUnknown emptyMap
  where withUnknown = Map.insert unknownCategory (createEmptyCategory unknownCategory)
        emptyMap = Map.fromList . List.map toTuple $ defs
        toTuple p = (getParserName p, createEmptyCategory (getParserName p))

createEmptyCategory :: String -> Category
createEmptyCategory name = Category { name=name, commits=[] }

addCommitToCategory :: Commit -> Category -> Category
addCommitToCategory c ctg = Category { name=name ctg,
                                       commits=c:commits ctg
                                     }

categoryToStatLine :: Category -> [String]
categoryToStatLine c = name c : List.foldr showStat [] accs
  where showStat x y = (show . x) c:y
        accs = [length . commits]

sortCategoriesByCommits :: [Category] -> [Category]
sortCategoriesByCommits = sortByAccessorDesc $ length . commits
