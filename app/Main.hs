module Main (main) where

import Boards (blank)
import Data.List (intercalate)
import Lib (Grid, solve)
import System.IO (hFlush, stdout)

process :: [Grid] -> IO ()
process [] = print "No Elements to process"
process (x : xs) = do
  putStrLn "Next Board:\n"
  putStrLn (intercalate "\n" x)
  putStrLn "\nContinue? (y/n)"
  hFlush stdout
  choice <- getLine
  case choice of
    "y" -> process xs
    "n" -> print "Stopping."
    _ -> print "Invalid choice. Stopping."

main :: IO ()
main = process (solve blank)
