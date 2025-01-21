import Data.Map qualified as Map
import Impose (Paper (..))
import Impose qualified
import Test.Hspec


main :: IO ()
main = hspec $ do
  describe "Impose util" $ do
    let totalPages = 16

    it "paper from page 1" $ do
      Impose.toPaper 1 totalPages
        `shouldBe` Paper
          { frontLeft = 2
          , frontRight = 15
          , backLeft = 16
          , backRight = 1
          }

    it "paper from page 2" $ do
      Impose.toPaper 2 totalPages
        `shouldBe` Paper
          { frontLeft = 4
          , frontRight = 13
          , backLeft = 14
          , backRight = 3
          }

    it "paper from page 3" $ do
      Impose.toPaper 3 totalPages
        `shouldBe` Paper
          { frontLeft = 6
          , frontRight = 11
          , backLeft = 12
          , backRight = 5
          }

    it "gens papers" $ do
      Impose.toIndex 16
        `shouldBe` Map.fromList
          [ (1, 1)
          , (2, 1)
          , (3, 2)
          , (4, 2)
          , (5, 3)
          , (6, 3)
          , (7, 4)
          , (8, 4)
          , (9, 4)
          , (10, 4)
          , (11, 3)
          , (12, 3)
          , (13, 2)
          , (14, 2)
          , (15, 1)
          , (16, 1)
          ]
