import Data.Map qualified as Map
import Impose (Paper (..), PaperPosition (..))
import Impose qualified
import Test.Hspec


main :: IO ()
main = hspec $ do
  describe "Impose util" $ do
    let signatureSize = 4

    it "paper from page 1" $ do
      Impose.toPaper 1 signatureSize
        `shouldBe` Paper
          { frontLeft = 2
          , frontRight = 15
          , backLeft = 16
          , backRight = 1
          }

    it "paper from page 2" $ do
      Impose.toPaper 2 signatureSize
        `shouldBe` Paper
          { frontLeft = 4
          , frontRight = 13
          , backLeft = 14
          , backRight = 3
          }

    it "paper from page 3" $ do
      Impose.toPaper 3 signatureSize
        `shouldBe` Paper
          { frontLeft = 6
          , frontRight = 11
          , backLeft = 12
          , backRight = 5
          }

    it "paper from 1 page" $ do
      Impose.toPaper 1 1
        `shouldBe` Paper
          { frontLeft = 2
          , frontRight = 3
          , backLeft = 4
          , backRight = 1
          }

    it "gens papers with 16 pages" $ do
      Impose.toIndex 4
        `shouldBe` Map.fromList
          [ (1, (1, BackRight))
          , (2, (1, FrontLeft))
          , (3, (2, BackRight))
          , (4, (2, FrontLeft))
          , (5, (3, BackRight))
          , (6, (3, FrontLeft))
          , (7, (4, BackRight))
          , (8, (4, FrontLeft))
          , (9, (4, FrontRight))
          , (10, (4, BackLeft))
          , (11, (3, FrontRight))
          , (12, (3, BackLeft))
          , (13, (2, FrontRight))
          , (14, (2, BackLeft))
          , (15, (1, FrontRight))
          , (16, (1, BackLeft))
          ]

    it "list to position off by 0" $ do
      Impose.listToPosition 0 1 ["1"] `shouldBe` [("1", Just (1, BackRight))]

    it "list to position off by 1" $ do
      Impose.listToPosition 1 1 ["1"] `shouldBe` [("1", Just (1, FrontLeft))]

    it "list to position off by 2" $ do
      Impose.listToPosition 2 1 ["1"] `shouldBe` [("1", Just (1, FrontRight))]

    it "list to position off by 3" $ do
      Impose.listToPosition 3 1 ["1"] `shouldBe` [("1", Just (1, BackLeft))]

    it "list to position off by 0, 4 pages" $ do
      Impose.listToPosition 0 1 ["1", "2", "3", "4"]
        `shouldBe` [ ("1", Just (1, BackRight))
                   , ("2", Just (1, FrontLeft))
                   , ("3", Just (1, FrontRight))
                   , ("4", Just (1, BackLeft))
                   ]

    it "list to position off by 1, 4 pages" $ do
      Impose.listToPosition 1 2 ["1", "2", "3", "4"]
        `shouldBe` [ ("1", Just (1, FrontLeft))
                   , ("2", Just (2, BackRight))
                   , ("3", Just (2, FrontLeft))
                   , ("4", Just (2, FrontRight))
                   ]

    it "list to position off by 2, 4 pages" $ do
      Impose.listToPosition 2 2 ["1", "2", "3", "4"]
        `shouldBe` [ ("1", Just (2, BackRight))
                   , ("2", Just (2, FrontLeft))
                   , ("3", Just (2, FrontRight))
                   , ("4", Just (2, BackLeft))
                   ]
