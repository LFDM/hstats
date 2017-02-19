module Commit
( Commit
, createCommit
, getCommitAuthor
, getCommitFiles
) where

import FileStat

data Commit = Commit { author :: String
                     , sha :: String
                     , date :: String
                     , files :: [FileStat]
                     } deriving (Show)

createCommit :: String -> String -> String -> [FileStat] -> Commit
createCommit sha author date files = Commit { sha=sha
                                            , author=author
                                            , date=date
                                            , files=files
                                            }

getCommitAuthor :: Commit -> String
getCommitAuthor = author

getCommitFiles :: Commit -> [FileStat]
getCommitFiles = files

