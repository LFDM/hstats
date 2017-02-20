module FileStat
( FileStat
, createFileStat
, getFSPath
, getFSChanges
, getFSAdditions
, getFSDeletions
) where

data FileStat = FileStat { path :: FilePath
                         , additions :: Int
                         , deletions :: Int
                         } deriving (Show)

createFileStat :: String -> String -> String -> FileStat
createFileStat a d p = FileStat {additions=read a :: Int, deletions=read d :: Int, path=p}

getFSPath :: FileStat -> String
getFSPath = path

getFSChanges :: FileStat -> (Int, Int)
getFSChanges f = (additions f, deletions f)

getFSAdditions :: FileStat -> Int
getFSAdditions = additions

getFSDeletions :: FileStat -> Int
getFSDeletions = deletions

