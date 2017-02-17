module Loc
( countLinesAtPath
) where

import System.Environment
import System.IO
import System.Directory
import Control.Monad
import Data.List

import System.FilePath (takeExtension)

supportedExts = [".js", ".hs", ".yaml"]

isEmpty :: String -> Bool
isEmpty x = length x == 0

countLinesAtPath  :: String -> IO ()
countLinesAtPath path = do
  count <- countLinesInDirectory path
  print count
  return ()

-- allow paths to files directly, check first if file exists

-- create Records for each parserDef - can and shall be read from an additional
-- file to define custom matchers for filetypes
-- map over files and create tuples like (filetype, code line, blank line, comment)
-- combine these after wards and render

countLinesInDirectory :: String -> IO Int
countLinesInDirectory path = do
    allFiles <- getFilePaths path
    acceptedFiles <- filterM (\name -> return $ isAcceptedPath name) allFiles
        >>= mapM (return . (toAbsolutePath path))

    print $ length allFiles
    print $ length acceptedFiles

    print $ diffLength allFiles acceptedFiles

    lineCounts <- mapM count acceptedFiles
    return $ sum lineCounts
    where
        count name = do
            b <- doesFileExist name
            if b
                then countLinesInFile name
                else countLinesInDirectory name

getFilePaths :: FilePath -> IO [FilePath]
getFilePaths path = do
    isFile <- doesFileExist path
    if isFile
      then return $ [path]
      else getFilePathsInDirectory path

getFilePathsInDirectory :: FilePath-> IO [FilePath]
getFilePathsInDirectory path = contents >>= mapM (return . (toAbsolutePath path))
  where contents = getDirectoryContents path

toAbsolutePath :: FilePath -> FilePath -> FilePath
toAbsolutePath base other = (base ++ "/" ++ other)


isAcceptedPath :: String -> Bool
isAcceptedPath = isAcceptedExtension . takeExtension

isAcceptedExtension :: String -> Bool
isAcceptedExtension x = x == "" || x `elem` supportedExts

diffLength :: [a] -> [a] -> Int
diffLength x y = length x - length y

countLinesInFile filename = do
    content <- readFile filename
    return $ length $ lines content
