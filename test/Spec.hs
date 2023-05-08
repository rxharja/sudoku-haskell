import Lib (Grid, boxes, cols, rows, valid)
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

genRow :: Gen [Char]
genRow = do
  let els = concat ("." : map show ([1 .. 9] :: [Int]))
  vectorOf 9 (elements els)

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
