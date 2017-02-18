module Lib
( run
) where

import System.Environment as Env
import System.IO
import Data.List
import Data.Maybe
import Printer
import Loc as L

type Command = [String] -> IO ()

commandList :: [(String, Command)]
commandList = [ ("help", help)
              , ("changes", changes)
              , ("loc", loc)
              ]

findCommand :: String -> Command
findCommand cmd = fromMaybe unknown $ lookup cmd commandList

executeCommand :: String -> [String] -> IO ()
executeCommand = findCommand

run = do
  (cmd:args) <- Env.getArgs
  executeCommand cmd args

help :: Command
help = logCmd "help"

changes :: Command
changes = logCmd "changes"

loc :: Command
loc [] = L.printLineCountsAtPath "."
loc (x:xs) = L.printLineCountsAtPath x


unknown :: Command
unknown args = putStr $ nl (inRed "Unknown command")

logCmd :: String -> Command
logCmd cmd args = putStr $ nl (unwords output)
  where output = [ "calling"
                 , inGreen cmd
                 , "with"
                 , inYellow $ unwords args
                 ]

