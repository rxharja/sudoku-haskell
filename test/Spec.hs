import Boards (blank, easy, easySolution, hard, medium)
import Data.List (intercalate, nub)
import Lib (Choices, Grid, Row, boxes, cols, consistent, nodups, replace, rows, solve)
import Test.Hspec (context, describe, hspec, it, shouldBe)
import Test.QuickCheck (Arbitrary, Gen, Property, arbitrary, elements, forAll, property, suchThat, vectorOf)

genInt :: Int -> Gen Int
genInt x = elements [0 .. x]

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

prop_replace_tailsMatch :: Property
prop_replace_tailsMatch = forAll genRow $ \r -> do
  x <- genInt (length r)
  let newList = replace r x 'h'
  return $ drop (x + 1) r == drop (x + 1) newList

prop_replace_headsMatch :: Property
prop_replace_headsMatch = forAll genRow $ \r -> do
  x <- genInt (length r)
  let newList = replace r x 'h'
  return $ take x r == take x newList

prop_replace_valueChanged :: Property
prop_replace_valueChanged = forAll genRow $ \r -> do
  x <- genInt (length r)
  let newList = replace r x 'h'
  return $ (newList !! x) == 'h'

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

  describe "replace" $ do
    context "when applied to a list" $ do
      it "should return a new list with a new value at the chosen index" $ do
        property prop_replace_valueChanged
      it "should return a new list that has the same tail as the original from the given index" $ do
        property prop_replace_tailsMatch
      it "should return a new list that has the same head as the original from the given index" $ do
        property prop_replace_headsMatch

  describe "Solving" $ do
    let solvedEasy = solve easy
    it "the easy puzzle should produce one valid solution" $ do
      length solvedEasy `shouldBe` 1
    it ("the easy puzzle should be " ++ show (intercalate "\n" easySolution)) $ do
      show solvedEasy `shouldBe` show [easySolution]

    let solvedMedium = solve medium
    it "the medium puzzle should produce one valid solution" $ do
      length solvedMedium `shouldBe` 1

    let solvedHard = solve hard
    it "the hard puzzle should produce one valid solution" $ do
      length solvedHard `shouldBe` 1

    let blanks = solve blank
    it "the blank puzzle should produce at least one valid solution" $ do
      length (take 2 blanks) > 1 `shouldBe` True
