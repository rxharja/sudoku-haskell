import Lib (Grid, Row, boxes, cols, nodups, rows, solve, valid)
import Data.List (intercalate)
import Test.Hspec (describe, hspec, it)
import Test.QuickCheck (Gen, Property, elements, forAll, quickCheck, vectorOf)

blank :: Grid
blank = replicate 9 $ replicate 9 '.'

easy :: Grid
easy =
  [ "2....1.38",
    "........5",
    ".7...6...",
    ".......13",
    ".981..257",
    "31....8..",
    "9..8...2.",
    ".5..69784",
    "4..25...."
  ]

medium :: Grid
medium =
  [ ".1.42...5",
    "..2.71.39",
    ".......4.",
    "2.71....6",
    "....4....",
    "6....74.3",
    ".7.......",
    "12.73.5..",
    "3...82.7."
  ]

hard :: Grid
hard =
  [ ".9.7..86.",
    ".31..5.2.",
    "8.6......",
    "..7.5...6",
    "...3.7...",
    "5...1.7..",
    "......1.9",
    ".2.6..35.",
    ".54..8.7."
  ]

genRow :: Gen (Row Char)
genRow = vectorOf 9 $ elements ('.' : ['1' .. '9'])

genGrid :: Gen Grid
genGrid = vectorOf 9 genRow

prop_row :: Property
prop_row = forAll genGrid $ (==) <*> rows

prop_rows :: Property
prop_rows = forAll genGrid $ (==) <*> rows . rows

prop_col :: Property
prop_col = forAll genGrid $ (/=) <*> cols

prop_cols :: Property
prop_cols = forAll genGrid $ (==) <*> cols . cols

prop_box :: Property
prop_box = forAll genGrid $ (/=) <*> boxes

prop_boxes :: Property
prop_boxes = forAll genGrid $ (==) <*> boxes . boxes

format :: [[String]] -> IO ()
format = putStrLn . intercalate "\n" . head 

main :: IO ()
main = hspec $ do
  describe "Sudoku Row, Column, and Box Properties" $ do
    it "Applying rows once on a grid SHOULD be the same as applying identity" $ do
      quickCheck prop_row
    it "Applying rows twice on a grid SHOULD be the same as applying identity" $ do
      quickCheck prop_rows

    it "Applying cols once on a grid SHOULD NOT be the same as applying identity" $ do
      quickCheck prop_col
    it "Applying cols twice on a grid SHOULD be the same as applying identity" $ do
      quickCheck prop_cols

    it "Applying boxes once on a grid SHOULD NOT be the same as applying identity" $ do
      quickCheck prop_box
    it "Applying boxes twice on a grid SHOULD be the same as applying identity" $ do
      quickCheck prop_boxes
  describe "Sudoku Function Validation" $ do
    it "Applying nodups to a row of 1..9 should return true" $ do
      nodups ['1' .. '9']
    it "Applying nodups to a row with a duplicate 9 should return false" $ do
      not (nodups $ '9' : ['1' .. '9'])
  describe "Solving Sudoku" $ do
    it "Solving the easy puzzle should produce one valid solution" $ do
      format . solve $ easy
    it "Solving the medium puzzle should produce one valid solution" $ do
      format . solve $ medium
    it "Solving the hard puzzle should produce one valid solution" $ do
      format . solve $ hard
    it "Solving the blank puzzle should produce at least one solution" $ do
      format . solve $ blank
