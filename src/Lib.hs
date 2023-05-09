module Lib (Grid, Row, rows, cols, boxes, valid, nodups, solve) where

import Data.List (delete, transpose)

type Grid = Matrix Value

type Matrix a = [Row a]

type Choices = [Value]

type Row a = [a]

type Value = Char

singleton :: [a] -> Bool
singleton xs = length xs == 1

rows :: Matrix a -> [Row a]
rows = id

cols :: Matrix a -> [Row a]
cols = transpose

boxes :: Matrix a -> [Row a]
boxes = unpack . map cols . pack
  where
    pack = split . map split
    split = chop 3
    unpack = map concat . concat
    chop _ [] = []
    chop n xs = take n xs : chop n (drop n xs)

valid :: Grid -> Bool
valid g = and $ all nodups <$> ([rows, cols, boxes] <*> pure g)

nodups :: Eq a => [a] -> Bool
nodups [] = True
nodups (x : xs) = notElem x xs && nodups xs

solve :: Grid -> [Grid]
solve = filter valid . explode . fix prune . choices

choices :: Grid -> Matrix Choices
choices = map (map choice) where choice v = if v == '.' then ['1' .. '9'] else [v]

cp :: [[a]] -> [[a]] -- cartesian product
cp [] = [[]]
cp (xs : xss) = [y : ys | y <- xs, ys <- cp xss]

explode :: Matrix [a] -> [Matrix a] -- cartesian explosion of matrices
explode m = cp (map cp m)

prune :: Matrix Choices -> Matrix Choices -- reduce the problem space
prune = pruneBy boxes . pruneBy cols . pruneBy rows
  where
    pruneBy f = f . map eliminate . f
    eliminate xss = [process xs xss | xs <- xss]
    process xs xss = if singleton xs then xs else foldr (delete . head) xs (filter singleton xss)

fix :: Eq a => (a -> a) -> a -> a
fix f x = if x == x' then x else fix f x' where x' = f x
