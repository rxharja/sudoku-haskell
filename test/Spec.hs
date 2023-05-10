import Data.List (intercalate, nub)
import Lib (Choices, Grid, Row, boxes, cols, consistent, nodups, rows, solve)
import Test.Hspec (after, context, describe, hspec, it, shouldBe)
import Test.QuickCheck (Arbitrary, Gen, Property, arbitrary, elements, forAll, property, suchThat, vectorOf)

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

genUniqList :: (Eq a, Arbitrary a) => Gen [a]
genUniqList = nub <$> arbitrary

genListWithDuplicate :: (Arbitrary a) => Gen [a]
genListWithDuplicate = do
  xs <- vectorOf 9 arbitrary `suchThat` (not . null)
  let (ys, zs) = splitAt (9 `div` 2) xs
      duplicate = if null zs then head ys else head zs
  return $ ys ++ [duplicate] ++ zs

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
  describe "row" $ do
    context "when applied" $ do
      it "once should be the same as applying identity" $ do
        property prop_row
      it "twice should be the same as applying identity" $ do
        property prop_rows

  describe "col" $ do
    context "when applied" $ do
      it "once SHOULD NOT be the same as applying identity" $ do
        property prop_col
      it "twice SHOULD be the same as applying identity" $ do
        property prop_cols

  describe "Box" $ do
    context "when applied" $ do
      it "once SHOULD NOT be the same as applying identity" $ do
        property prop_box
      it "twice SHOULD be the same as applying identity" $ do
        property prop_boxes

  describe "nodups" $ do
    context "when applied to a list of" $ do
      it "unique values ([] is unique) should return true" $ do
        property $ forAll (genUniqList :: Gen (Row Char)) nodups
      it "non-unique values ([] is unique) should return false" $ do
        property $ forAll (genListWithDuplicate :: Gen (Row Char)) $ not . nodups

  describe "consistent" $ do
    context "when applied to a list of" $ do
      it "unique values should return true" $ do
        property $ forAll (genUniqList :: Gen (Row Choices)) consistent

  describe "Solving" $ do
    it "the easy puzzle should produce one valid solution" $ do
      let solutions = solve easy
      length solutions `shouldBe` 1

-- it "Solving the medium puzzle should produce one valid solution" $ do
--   format . solve $ medium
-- it "Solving the hard puzzle should produce one valid solution" $ do
--   format . solve $ hard
-- it "Solving the blank puzzle should produce at least one solution" $ do
--   format . solve $ blank
