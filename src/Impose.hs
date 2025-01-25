{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Impose where

import Data.Foldable (traverse_)
import Data.List qualified as List
import Data.List.Split as List.Split
import Data.Map qualified as Map
import Debug.Trace qualified as Debug
import GHC.Float (int2Float)
import Options.Applicative qualified as OptParse
import System.FilePath ((</>))
import System.FilePath qualified as FilePath
import System.FilePath.Glob qualified as Glob


-- MAIN

main :: IO ()
main = do
  config <- parseArgs
  print config
  pages <- fromInputDirOrdered config
  let signatureSize =
        case config.signatureSizeOption of
          Auto -> SignatureSize $ ceiling $ int2Float (length pages) / 4
          Custom n -> n
      positions = listToPosition config.offset signatureSize pages
  traverse_ print positions


-- DEFS

data PaperSheet
  = PaperSheet
  { frontLeft :: !PageNumber
  , frontRight :: !PageNumber
  , backLeft :: !PageNumber
  , backRight :: !PageNumber
  , number :: !PaperSheetNumber
  }
  deriving (Show, Eq)


data PositionOnPaperSheet
  = FrontLeft
  | FrontRight
  | BackLeft
  | BackRight
  deriving (Show, Eq)


newtype SignatureSize
  = SignatureSize {value :: Int}
  deriving (Show, Eq, Ord)


newtype PaperSheetNumber
  = PaperSheetNumber {value :: Int}
  deriving (Show, Eq, Ord)


newtype PageAmount
  = PageAmount {value :: Int}
  deriving (Show, Eq, Ord)


newtype PageNumber
  = PageNumber {value :: Int}
  deriving (Show, Eq, Ord)


newtype SignatureIndex
  = SignatureIndex
  { value ::
      Map.Map PageNumber (PaperSheetNumber, PositionOnPaperSheet)
  }
  deriving (Show, Eq)
  deriving newtype (Semigroup, Monoid)


-- FUN

fromInputDirOrdered :: Config -> IO [FilePath]
fromInputDirOrdered config = do
  pages <- Glob.glob $ config.inputDir </> "*.tif"
  pure
    $ List.sortBy
      (\a b -> compare (FilePath.takeFileName a) (FilePath.takeFileName b))
    $ filter (FilePath.isExtensionOf "tif") pages


toPaperSheetData :: PaperSheetNumber -> SignatureSize -> PaperSheet
toPaperSheetData paperSheetNumber signatureSize =
  PaperSheet
    (PageNumber frontLeft)
    (PageNumber frontRight)
    (PageNumber backLeft)
    (PageNumber backRight)
    paperSheetNumber
 where
  frontLeft = backRight + 1
  frontRight = backLeft - 1
  backLeft = total - backRight + 1
  backRight = (paperSheetNumber.value - 1) * 2 + 1
  total = signatureSize.value * 4


toSignatureIndex :: SignatureSize -> SignatureIndex
toSignatureIndex signatureSize =
  SignatureIndex $
    foldMap
      ( \paperSheetNumber ->
          let
            sheetData = toPaperSheetData paperSheetNumber signatureSize
           in
            Map.fromList
              [ (sheetData.backLeft, (sheetData.paperSheetNumber, BackLeft))
              , (sheetData.backRight, (sheetData.paperSheetNumber, BackRight))
              , (sheetData.frontLeft, (sheetData.paperSheetNumber, FrontLeft))
              , (sheetData.frontRight, (sheetData.paperSheetNumber, FrontRight))
              ]
      )
      ixs
 where
  ixs = take signatureSize.value $ PaperSheetNumber <$> [1 ..]


listToSignatureSizes :: [a] -> SignatureSize -> [SignatureSize]
listToSignatureSizes [] _ = []
listToSignatureSizes xs signatureSize =
  let
    needed = length xs
    optioned = signatureSize.value * 4
   in
    if needed < optioned
      then [SignatureSize $ needed `mod` 4]
      else signatureSize : listToSignatureSizes (drop optioned xs) signatureSize


generateSignatureIndecies :: [SignatureSize] -> [(Int, SignatureIndex)]
generateSignatureIndecies =
  zip [1 ..] . fmap toSignatureIndex


listToPosition ::
  Int ->
  SignatureSize ->
  [FilePath] ->
  [(FilePath, Maybe (Int, PositionOnPaperSheet))]
listToPosition offset signatureSize xs =
  [(x, Map.lookup ix index.value) | (x, ix) <- xsWithIndex]
 where
  index = toSignatureIndex signatureSize
  xsWithIndex = zip xs [offset + 1 ..]


-- CLI

data Config
  = Config
  { inputDir :: !FilePath
  , signatureSizeOption :: !SignatureSizeOption
  , offset :: !Int
  }
  deriving (Show, Eq)


data SignatureSizeOption = Auto | Custom !SignatureSize
  deriving (Show, Eq)


parseArgs :: IO Config
parseArgs =
  OptParse.customExecParser (OptParse.prefs OptParse.showHelpOnError) $
    OptParse.info
      (parser OptParse.<**> OptParse.helper)
      (OptParse.fullDesc <> OptParse.progDesc "impose")


parser :: OptParse.Parser Config
parser =
  Config
    <$> OptParse.strOption
      ( OptParse.long "input"
          <> OptParse.short 'i'
          <> OptParse.metavar "DIR"
      )
    <*> parseSignatureSizeOption
    <*> parseOffset


parseSignatureSizeOption :: OptParse.Parser SignatureSizeOption
parseSignatureSizeOption =
  OptParse.option (fmap (Custom . SignatureSize) OptParse.auto) $
    OptParse.long "signatureSize"
      <> OptParse.short 's'
      <> OptParse.showDefault
      <> OptParse.value Auto
      <> OptParse.metavar "INT"
      <> OptParse.help "How many papers per signature"


parseOffset :: OptParse.Parser Int
parseOffset =
  OptParse.option OptParse.auto $
    OptParse.long "offset"
      <> OptParse.short 'o'
      <> OptParse.showDefault
      <> OptParse.value 0
      <> OptParse.metavar "INT"
      <> OptParse.help "At what page does the book start"
