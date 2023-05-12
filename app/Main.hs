module Main (main) where

import Lib (Grid, format, solve)

blank :: Grid
blank = replicate 9 (replicate 9 '.')

main :: IO ()
main = do format $ solve blank
