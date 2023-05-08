module Lib (Grid, Row, rows, cols, boxes, valid, solve) where

import Data.List (transpose)

type Grid = Matrix Value

type Matrix a = [Row a]

type Row a = [a]

type Value = Char

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
solve = filter valid . collapse . choices

choices = undefined

collapse = undefined
