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
    it "Solving the easy solution should produce exactly one valid solution" $ do
      print (solve easy)
