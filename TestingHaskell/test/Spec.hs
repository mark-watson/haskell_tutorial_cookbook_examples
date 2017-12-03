import Test.Hspec

import MyColors

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "head" $ do
    it "test removing first list element" $ do
      head [1,2,3,4] `shouldBe` 1
      head ["the", "dog", "ran"] `shouldBe` "dog" -- should fail
  describe "MyColors tests" $ do
    it "test custom 'compare' function descending test" $ do
      MyColors.Green < MyColors.Red `shouldBe` True
    it "test custom 'compare' function ascending test" $ do
      Red > Silver `shouldBe` False
  