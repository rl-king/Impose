import Data.Map qualified as Map
import Impose hiding (main)
import Test.Hspec


main :: IO ()
main = hspec $ do
  describe "Impose util" $ do
    let signatureSize = SignatureSize 4

    it "paper from page 1" $ do
      Impose.toPaperSheetData (SheetNumber 1) signatureSize
        `shouldBe` PaperSheet
          { frontLeft = PageNumber 2
          , frontRight = PageNumber 15
          , backLeft = PageNumber 16
          , backRight = PageNumber 1
          }

    it "paper from page 2" $ do
      Impose.toPaperSheetData (SheetNumber 2) signatureSize
        `shouldBe` PaperSheet
          { frontLeft = PageNumber 4
          , frontRight = PageNumber 13
          , backLeft = PageNumber 14
          , backRight = PageNumber 3
          }

    it "paper from page 3" $ do
      Impose.toPaperSheetData (SheetNumber 3) signatureSize
        `shouldBe` PaperSheet
          { frontLeft = PageNumber 6
          , frontRight = PageNumber 11
          , backLeft = PageNumber 12
          , backRight = PageNumber 5
          }

    it "paper from 1 page" $ do
      Impose.toPaperSheetData (SheetNumber 1) (SignatureSize 1)
        `shouldBe` PaperSheet
          { frontLeft = PageNumber 2
          , frontRight = PageNumber 3
          , backLeft = PageNumber 4
          , backRight = PageNumber 1
          }

    it "gens papers with 16 pages" $ do
      Impose.toSignatureIndex (SignatureNumber 1) (SignatureSize 4)
        `shouldBe` ( Impose.SignatureIndex $
                       Map.fromList
                         [ (PageNumber 1, (SignatureNumber 1, SheetNumber 1, BackRight))
                         , (PageNumber 2, (SignatureNumber 1, SheetNumber 1, FrontLeft))
                         , (PageNumber 3, (SignatureNumber 1, SheetNumber 2, BackRight))
                         , (PageNumber 4, (SignatureNumber 1, SheetNumber 2, FrontLeft))
                         , (PageNumber 5, (SignatureNumber 1, SheetNumber 3, BackRight))
                         , (PageNumber 6, (SignatureNumber 1, SheetNumber 3, FrontLeft))
                         , (PageNumber 7, (SignatureNumber 1, SheetNumber 4, BackRight))
                         , (PageNumber 8, (SignatureNumber 1, SheetNumber 4, FrontLeft))
                         , (PageNumber 9, (SignatureNumber 1, SheetNumber 4, FrontRight))
                         , (PageNumber 10, (SignatureNumber 1, SheetNumber 4, BackLeft))
                         , (PageNumber 11, (SignatureNumber 1, SheetNumber 3, FrontRight))
                         , (PageNumber 12, (SignatureNumber 1, SheetNumber 3, BackLeft))
                         , (PageNumber 13, (SignatureNumber 1, SheetNumber 2, FrontRight))
                         , (PageNumber 14, (SignatureNumber 1, SheetNumber 2, BackLeft))
                         , (PageNumber 15, (SignatureNumber 1, SheetNumber 1, FrontRight))
                         , (PageNumber 16, (SignatureNumber 1, SheetNumber 1, BackLeft))
                         ]
                   )
    it "list to position off by 0" $ do
      Impose.listToPosition (Offset (PageAmount 0)) (SignatureSize 1) ["1"]
        `shouldBe` [ PageData "1" (PageNumber 1) (SignatureNumber 1) (SheetNumber 1) BackRight
                   ]

    it "list to position off by 1" $ do
      Impose.listToPosition (Offset (PageAmount 1)) (SignatureSize 1) ["1"]
        `shouldBe` [PageData "1" (PageNumber 2) (SignatureNumber 1) (SheetNumber 1) FrontLeft]

    it "list to position off by 2" $ do
      Impose.listToPosition (Offset (PageAmount 2)) (SignatureSize 1) ["1"]
        `shouldBe` [ PageData "1" (PageNumber 3) (SignatureNumber 1) (SheetNumber 1) FrontRight
                   ]

    it "list to position off by 3" $ do
      Impose.listToPosition (Offset (PageAmount 3)) (SignatureSize 1) ["1"]
        `shouldBe` [ PageData "1" (PageNumber 4) (SignatureNumber 1) (SheetNumber 1) BackLeft
                   ]

    it "list to sig 1" $ do
      Impose.listToSignatureSizes (PageAmount 3) (SignatureSize 1)
        `shouldBe` [SignatureSize 1]

    it "list to sig 2" $ do
      Impose.listToSignatureSizes (PageAmount 3) (SignatureSize 2)
        `shouldBe` [SignatureSize 1]

    it "list to sig 3" $ do
      Impose.listToSignatureSizes (PageAmount 5) (SignatureSize 2)
        `shouldBe` [SignatureSize 2]

    it "list to sig 4" $ do
      Impose.listToSignatureSizes (PageAmount 16) (SignatureSize 2)
        `shouldBe` [SignatureSize 2, SignatureSize 2]

    it "list to sig 5" $ do
      Impose.listToSignatureSizes (PageAmount 17) (SignatureSize 2)
        `shouldBe` [SignatureSize 2, SignatureSize 2, SignatureSize 1]

    it "list to position off by 0, 4 pages" $ do
      Impose.listToPosition
        (Offset (PageAmount 0))
        (SignatureSize 1)
        (show @Int <$> [1 .. 4])
        `shouldBe` [ PageData
                       "1"
                       (PageNumber 1)
                       (SignatureNumber 1)
                       (SheetNumber 1)
                       BackRight
                   , PageData
                       "2"
                       (PageNumber 2)
                       (SignatureNumber 1)
                       (SheetNumber 1)
                       FrontLeft
                   , PageData
                       "3"
                       (PageNumber 3)
                       (SignatureNumber 1)
                       (SheetNumber 1)
                       FrontRight
                   , PageData
                       "4"
                       (PageNumber 4)
                       (SignatureNumber 1)
                       (SheetNumber 1)
                       BackLeft
                   ]

    it "list to position off by 1, 4 pages" $ do
      Impose.listToPosition
        (Offset (PageAmount 1))
        (SignatureSize 2)
        (show @Int <$> [1 .. 4])
        `shouldBe` [ PageData "1" (PageNumber 2) (SignatureNumber 1) (SheetNumber 1) FrontLeft
                   , PageData "2" (PageNumber 3) (SignatureNumber 1) (SheetNumber 2) BackRight
                   , PageData "3" (PageNumber 4) (SignatureNumber 1) (SheetNumber 2) FrontLeft
                   , PageData "4" (PageNumber 5) (SignatureNumber 1) (SheetNumber 2) FrontRight
                   ]

    it "list to position off by 2, 4 pages" $ do
      Impose.listToPosition
        (Offset (PageAmount 2))
        (SignatureSize 2)
        (show @Int <$> [1 .. 4])
        `shouldBe` [ PageData "1" (PageNumber 3) (SignatureNumber 1) (SheetNumber 2) BackRight
                   , PageData "2" (PageNumber 4) (SignatureNumber 1) (SheetNumber 2) FrontLeft
                   , PageData "3" (PageNumber 5) (SignatureNumber 1) (SheetNumber 2) FrontRight
                   , PageData "4" (PageNumber 6) (SignatureNumber 1) (SheetNumber 2) BackLeft
                   ]

    it "list to position off by 0, 6 pages" $ do
      Impose.listToPosition
        (Offset (PageAmount 0))
        (SignatureSize 1)
        (show @Int <$> [1 .. 6])
        `shouldBe` [ PageData "1" (PageNumber 1) (SignatureNumber 1) (SheetNumber 1) BackRight
                   , PageData "2" (PageNumber 2) (SignatureNumber 1) (SheetNumber 1) FrontLeft
                   , PageData "3" (PageNumber 3) (SignatureNumber 1) (SheetNumber 1) FrontRight
                   , PageData "4" (PageNumber 4) (SignatureNumber 1) (SheetNumber 1) BackLeft
                   , PageData "5" (PageNumber 1) (SignatureNumber 2) (SheetNumber 1) BackRight
                   , PageData "6" (PageNumber 2) (SignatureNumber 2) (SheetNumber 1) FrontLeft
                   ]

    it "list to position off by 0, 8 pages, 2 sheet sig" $ do
      Impose.listToPosition
        (Offset (PageAmount 0))
        (SignatureSize 2)
        (show @Int <$> [1 .. 8])
        `shouldBe` [ PageData "1" (PageNumber 1) (SignatureNumber 1) (SheetNumber 1) BackRight
                   , PageData "2" (PageNumber 2) (SignatureNumber 1) (SheetNumber 1) FrontLeft
                   , PageData "3" (PageNumber 3) (SignatureNumber 1) (SheetNumber 2) BackRight
                   , PageData "4" (PageNumber 4) (SignatureNumber 1) (SheetNumber 2) FrontLeft
                   , PageData "5" (PageNumber 5) (SignatureNumber 1) (SheetNumber 2) FrontRight
                   , PageData "6" (PageNumber 6) (SignatureNumber 1) (SheetNumber 2) BackLeft
                   , PageData "7" (PageNumber 7) (SignatureNumber 1) (SheetNumber 1) FrontRight
                   , PageData "8" (PageNumber 8) (SignatureNumber 1) (SheetNumber 1) BackLeft
                   ]

    it "list to position off by 0, 16 pages, 2 sheet sig" $ do
      Impose.listToPosition
        (Offset (PageAmount 0))
        (SignatureSize 2)
        (show @Int <$> [1 .. 16])
        `shouldBe` [ PageData "1" (PageNumber 1) (SignatureNumber 1) (SheetNumber 1) BackRight
                   , PageData "2" (PageNumber 2) (SignatureNumber 1) (SheetNumber 1) FrontLeft
                   , PageData "3" (PageNumber 3) (SignatureNumber 1) (SheetNumber 2) BackRight
                   , PageData "4" (PageNumber 4) (SignatureNumber 1) (SheetNumber 2) FrontLeft
                   , PageData "5" (PageNumber 5) (SignatureNumber 1) (SheetNumber 2) FrontRight
                   , PageData "6" (PageNumber 6) (SignatureNumber 1) (SheetNumber 2) BackLeft
                   , PageData "7" (PageNumber 7) (SignatureNumber 1) (SheetNumber 1) FrontRight
                   , PageData "8" (PageNumber 8) (SignatureNumber 1) (SheetNumber 1) BackLeft
                   , PageData "9" (PageNumber 1) (SignatureNumber 2) (SheetNumber 1) BackRight
                   , PageData "10" (PageNumber 2) (SignatureNumber 2) (SheetNumber 1) FrontLeft
                   , PageData "11" (PageNumber 3) (SignatureNumber 2) (SheetNumber 2) BackRight
                   , PageData "12" (PageNumber 4) (SignatureNumber 2) (SheetNumber 2) FrontLeft
                   , PageData "13" (PageNumber 5) (SignatureNumber 2) (SheetNumber 2) FrontRight
                   , PageData "14" (PageNumber 6) (SignatureNumber 2) (SheetNumber 2) BackLeft
                   , PageData "15" (PageNumber 7) (SignatureNumber 2) (SheetNumber 1) FrontRight
                   , PageData "16" (PageNumber 8) (SignatureNumber 2) (SheetNumber 1) BackLeft
                   ]
