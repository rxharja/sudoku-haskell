module Lib (Grid, Row, Choices, rows, cols, boxes, valid, nodups, solve, void, safe, consistent, replace) where

import Data.List (delete, findIndex, transpose)
import Data.Maybe (fromMaybe)

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

choices :: Grid -> Matrix Choices
choices = map (map choice) where choice v = if v == '.' then ['1' .. '9'] else [v]

cp :: [[a]] -> [[a]] -- cartesian product
cp [] = [[]]
cp (xs : xss) = [y : ys | y <- xs, ys <- cp xss]

explode :: Matrix [a] -> [Matrix a] -- cartesian explosion of matrices
explode = cp . map cp

prune :: Matrix Choices -> Matrix Choices -- reduce the problem space
prune = pruneBy boxes . pruneBy cols . pruneBy rows
  where
    pruneBy f = f . map eliminate . f
    eliminate xss = [process xs xss | xs <- xss]
    process xs xss = if singleton xs then xs else foldr (delete . head) xs (filter singleton xss)

void :: Matrix Choices -> Bool
void = any (any null)

consistent :: Row Choices -> Bool
consistent = nodups . filter singleton

safe :: Matrix Choices -> Bool
safe m = and $ all consistent <$> ([rows, cols, boxes] <*> pure m)

blocked :: Matrix Choices -> Bool
blocked m = void m || (not . safe) m

search :: Matrix Choices -> [Grid]
search m
  | blocked m = []
  | all (all singleton) m = explode m
  | otherwise = [g | m' <- expand m, g <- search (prune m')]
  where
    expand xss = fromMaybe [] $ do
      ix <- findIndex (not . all singleton) xss
      rs <- expandRow (xss !! ix)
      return $ map (replace xss ix) rs
    expandRow xss = do
      ix <- findIndex (not . singleton) xss
      return $ map (replace xss ix . pure) (xss !! ix)

replace :: [a] -> Int -> a -> [a]
replace xs ix v = take ix xs ++ [v] ++ drop (ix + 1) xs

solve :: Grid -> [Grid]
solve = search . prune . choices
