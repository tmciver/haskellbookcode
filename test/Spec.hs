import Test.Hspec
import Chapter17.ZipList

main :: IO ()
main = hspec $ do
  describe "Applicative for ZipList'" $ do
    it "should return a list that is corresponding elements of two lists added together" $ do
      (+) <$> ZipList' (Cons 1 (Cons 2 (Cons 3 Nil))) <*> ZipList' (Cons 3 (Cons 2 (Cons 1 Nil))) `shouldBe` ZipList' (Cons 4 (Cons 4 (Cons 4 Nil)))
