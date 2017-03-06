module Commit
( Commit
, createCommit
, getCommitAuthor
, getCommitFiles
, getCommitMessage
, getCommitAdditions
, getCommitDeletions
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
                                                , date=date
                                                , msg=msg
                                                , files=files
                                                }

getCommitAuthor :: Commit -> String
getCommitAuthor = author

getCommitFiles :: Commit -> [FileStat]
getCommitFiles = files

getCommitMessage :: Commit -> String
getCommitMessage = msg

getCommitAdditions :: Commit -> Int
getCommitAdditions = sum . map getFSAdditions . files

getCommitDeletions :: Commit -> Int
getCommitDeletions = sum . map getFSDeletions . files

