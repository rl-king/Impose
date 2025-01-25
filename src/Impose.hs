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
  , number :: !SheetNumber
  }
  deriving (Show, Eq)


data PositionOnSheet
  = FrontLeft
  | FrontRight
  | BackLeft
  | BackRight
  deriving (Show, Eq)


newtype SignatureSize
  = SignatureSize {value :: Int}
  deriving (Show, Eq, Ord)


newtype SignatureNumber
  = SignatureNumber {value :: Int}
  deriving (Show, Eq, Ord)


newtype SheetNumber
  = SheetNumber {value :: Int}
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
      Map.Map PageNumber (SignatureNumber, SheetNumber, PositionOnSheet)
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


toPaperSheetData :: SheetNumber -> SignatureSize -> PaperSheet
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


toSignatureIndex :: SignatureNumber -> SignatureSize -> SignatureIndex
toSignatureIndex signatureNumber signatureSize =
  SignatureIndex $
    foldMap
      ( \paperSheetNumber ->
          let
            sheetData = toPaperSheetData paperSheetNumber signatureSize
           in
            Map.fromList
              [ (sheetData.backLeft, (signatureNumber, sheetData.number, BackLeft))
              , (sheetData.backRight, (signatureNumber, sheetData.number, BackRight))
              , (sheetData.frontLeft, (signatureNumber, sheetData.number, FrontLeft))
              , (sheetData.frontRight, (signatureNumber, sheetData.number, FrontRight))
              ]
      )
      ixs
 where
  ixs = take signatureSize.value $ SheetNumber <$> [1 ..]


listToSignatureSizes :: PageAmount -> SignatureSize -> [SignatureSize]
listToSignatureSizes (PageAmount 0) _ = []
listToSignatureSizes pageAmount signatureSize =
  let
    optioned = signatureSize.value * 4
   in
    if pageAmount.value < optioned
      then [SignatureSize . ceiling $ int2Float pageAmount.value / 4]
      else
        signatureSize
          : listToSignatureSizes (PageAmount (pageAmount.value - optioned)) signatureSize


generateSignatureIndex :: [SignatureSize] -> SignatureIndex
generateSignatureIndex =
  foldMap (uncurry toSignatureIndex) . zip (SignatureNumber <$> [1 ..])


listToPosition ::
  Offset ->
  SignatureSize ->
  [FilePath] ->
  [(FilePath, Maybe (SignatureNumber, SheetNumber, PositionOnSheet))]
listToPosition offset signatureSize xs =
  [(x, Map.lookup pageNumber signatureIndex.value) | (pageNumber, x) <- pageNumbered]
 where
  pageAmount = PageAmount $ length xs + offset.value.value
  sizes = listToSignatureSizes pageAmount signatureSize
  signatureIndex = generateSignatureIndex sizes
  pageNumbered = zip (PageNumber <$> [1 + offset.value.value ..]) xs


-- CLI

data Config
  = Config
  { inputDir :: !FilePath
  , signatureSizeOption :: !SignatureSizeOption
  , offset :: !Offset
  }
  deriving (Show, Eq)


newtype Offset = Offset {value :: PageAmount}
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


parseOffset :: OptParse.Parser Offset
parseOffset =
  OptParse.option (Offset . PageAmount <$> OptParse.auto) $
    OptParse.long "offset"
      <> OptParse.short 'o'
      <> OptParse.showDefault
      <> OptParse.value (Offset (PageAmount 0))
      <> OptParse.metavar "INT"
      <> OptParse.help "At what page does the book start"
