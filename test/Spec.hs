import Lib (Grid, Row, boxes, cols, rows, solve, valid)
import Test.Hspec
import Test.QuickCheck

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

prop_rows :: Property
prop_rows = forAll genGrid $ (==) <*> rows . rows

prop_cols :: Property
prop_cols = forAll genGrid $ (==) <*> cols . cols

prop_boxes :: Property
prop_boxes = forAll genGrid $ (==) <*> boxes . boxes

main :: IO ()
main = hspec $ do
  describe "Sudoku" $ do
    it "Applying rows twice on a grid should be the same as applying identity" $ do
      quickCheck prop_rows
    it "Applying cols twice on a grid should be the same as applying identity" $ do
      quickCheck prop_cols
    it "Applying boxes twice on a grid should be the same as applying identity" $ do
      quickCheck prop_boxes
    it "Solving the easy puzzle should produce one valid solution" $ do
      print (solve easy)
    it "Solving the medium puzzle should produce one valid solution" $ do
      print (solve medium)
    it "Solving the hard puzzle should produce one valid solution" $ do
      print (solve hard)
