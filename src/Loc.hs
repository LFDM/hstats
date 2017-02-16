module Loc
( countLinesAtPath
) where

import System.Environment
import System.IO
import System.Directory
import Control.Monad
import Data.List

countLinesAtPath  :: String -> IO ()
countLinesAtPath path = do
  count <- countLinesInDirectory path
  print count
  return ()

countLinesInDirectory :: String -> IO Int
countLinesInDirectory path = do
    filelist <- getDirectoryContents path
        >>= filterM (\name -> return $ name /= ".." && name /= ".")
        >>= mapM (return . (path_++))
    lineCounts <- mapM count filelist
    return $ sum lineCounts
    where
        count name = do
            b <- doesFileExist name
            if b
                then countLinesInFile name
                else countLinesInDirectory name
        path_ = path ++ "/"

countLinesInFile filename = do
    content <- readFile filename
    return $ length $ lines content
