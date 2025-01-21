import Impose (Paper (..))
import Impose qualified
import Test.Hspec


main :: IO ()
main = hspec $ do
  describe "Impose util" $ do
    let totalPages = 16

    it "paper from page 1" $ do
      Impose.paperToPages 1 totalPages
        `shouldBe` Paper
          { frontLeft = 2
          , frontRight = 15
          , backLeft = 16
          , backRight = 1
          }

    it "paper from page 2" $ do
      Impose.paperToPages 2 totalPages
        `shouldBe` Paper
          { frontLeft = 4
          , frontRight = 13
          , backLeft = 14
          , backRight = 3
          }

    it "paper from page 3" $ do
      Impose.paperToPages 3 totalPages
        `shouldBe` Paper
          { frontLeft = 6
          , frontRight = 11
          , backLeft = 12
          , backRight = 5
          }

    it "splits odd and even indecies" $ do
      Impose.oddEven @Int (take 6 [0 ..])
        `shouldBe` ([0, 2, 4], [1, 3, 5])
