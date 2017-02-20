module Contributor
( Contributor
, ContributorStats
, createContributor
, addCommitToContributor
, getContributorStats
, contributorToStatLine
) where

import Commit
import FileStat

type ContributorStats = (Int, Int, Int, Int)
data Contributor = Contributor { name :: String
                               , stats :: ContributorStats
                               , commits :: [Commit]
                               } deriving (Show)

getContributorStats :: Contributor -> ContributorStats
getContributorStats = stats

getContributorCommits :: Contributor -> [Commit]
getContributorCommits = commits

getContributorName :: Contributor -> String
getContributorName = name

createContributor :: String -> Contributor
createContributor n = Contributor {name=n, stats=(0, 0, 0, 0), commits=[]}

addCommitToContributor :: Commit -> Contributor -> Contributor
addCommitToContributor com c = Contributor {name=author, stats=stats, commits=commits}
  where author = getCommitAuthor com
        commits = com:(getContributorCommits c)
        stats = updateStats (getContributorStats c) (nextStats com)
        nextStats = foldr updateFS (0, 0, 0) . map getFSChanges . getCommitFiles
        updateStats (c, f1, a1, d1) (f2, a2, d2) = (c + 1, f1 + f2, a1 +  a2, d1 + d2)
        updateFS (a2, d2) (f1, a1, d1) = (f1 + 1, a1 + a2, d1 + d2)

contributorToStatLine :: Contributor -> [String]
contributorToStatLine c = (name c):((toStats . stats) c)
  where toStats (n, c, a, d) = map show [n, c, a, d, a - d, avg c a, avg c d, avg c (a - d)]
        avg x y = y `quot` x


