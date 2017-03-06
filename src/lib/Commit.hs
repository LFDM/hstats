module Commit
( Commit
, createCommit
, getCommitAuthor
, getCommitFiles
, getCommitMessage
) where

import Data.Function (on)
import FileStat

data Commit = Commit { author :: String
                     , sha :: String
                     , date :: String
                     , msg :: String
                     , files :: [FileStat]
                     } deriving (Show)

instance Eq Commit where
  x == y = ((==) `on` sha) x y

createCommit :: String -> String -> String -> String -> [FileStat] -> Commit
createCommit sha author date msg files = Commit { sha=sha
                                            , author=author
                                            , msg=msg
                                            , date=date
                                            , files=files
                                            }

getCommitAuthor :: Commit -> String
getCommitAuthor = author

getCommitFiles :: Commit -> [FileStat]
getCommitFiles = files

getCommitMessage :: Commit -> String
getCommitMessage = msg

