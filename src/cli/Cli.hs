module Cli
( run
) where

import System.Environment as Env
import System.IO
import Data.List
import Data.Maybe
import Printer
import Loc as Loc
import Hot as Hot

type Command = [String] -> IO ()

commandList :: [(String, Command)]
commandList = [ ("help", help)
              , ("changes", changes)
              , ("loc", loc)
              , ("hot", hot)
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
loc [] = Loc.printLineCountsAtPath "."
loc (x:xs) = Loc.printLineCountsAtPath x

hot :: Command
hot [] = Hot.printStats "1-week" "" 100 ""
hot (dur:[]) = Hot.printStats dur "" 100 ""
hot (dur:p:[]) = Hot.printStats dur p 100 ""
hot (dur:p:limit:[]) = Hot.printStats dur p (toInt limit) ""
hot (dur:p:limit:acc:[]) = Hot.printStats dur p (toInt limit) acc

unknown :: Command
unknown args = putStr $ nl (inRed "Unknown command")

logCmd :: String -> Command
logCmd cmd args = putStr $ nl (unwords output)
  where output = [ "calling"
                 , inGreen cmd
                 , "with"
                 , inYellow $ unwords args
                 ]

toInt :: String -> Int
toInt i = read i :: Int

