{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Impose where

import Data.Foldable (traverse_)
import Data.List qualified as List
import Data.Map qualified as Map
import Debug.Trace qualified as Debug
import GHC.Float (int2Float)
import Options.Applicative qualified as OptParse
import System.Directory.OsPath qualified as Dir
import System.FilePath.Glob qualified as Glob
import System.Log.FastLogger (LogType' (..), defaultBufSize)
import System.Log.FastLogger qualified as FastLogger
import System.OsPath (OsPath, (</>))
import System.OsPath qualified as OsPath
import Prelude hiding (log)


-- DEFS

data PageData a
  = PageData
  { content :: !a
  , pageNumber :: !PageNumber
  , signatureNumber :: !SignatureNumber
  , sheetNumber :: !SheetNumber
  , positionOnSheet :: !PositionOnSheet
  }
  deriving (Show, Eq)


data PaperSheet
  = PaperSheet
  { frontLeft :: !PageNumber
  , frontRight :: !PageNumber
  , backLeft :: !PageNumber
  , backRight :: !PageNumber
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


type Logger =
  FastLogger.LogStr -> IO ()


-- MAIN

main :: IO ()
main = do
  timeCache <- FastLogger.newTimeCache "%T"
  FastLogger.withTimedFastLogger timeCache (LogStdout defaultBufSize) $
    \timedFastLogger -> do
      let log :: (FastLogger.ToLogStr a) => a -> IO ()
          log = mkLog timedFastLogger . FastLogger.toLogStr
      config <- parseArgs
      print config
      pages <- fromInputDirOrdered config
      let signatureSize =
            case config.signatureSizeOption of
              Auto -> SignatureSize $ ceiling $ int2Float (length pages) / 4
              Custom n -> n
          positions = listToPosition config.offset signatureSize pages
      if config.dryRun
        then do
          log $ "SignatureSize: " <> show signatureSize
          traverse_ (log . show) positions
        else do
          log $ "SignatureSize: " <> show signatureSize
          copyFiles log positions


mkLog :: FastLogger.TimedFastLogger -> FastLogger.LogStr -> IO ()
mkLog logger msg =
  logger (\time -> FastLogger.toLogStr time <> " " <> msg <> "\n")


-- FILESYSTEM

fromInputDirOrdered :: Config -> IO [OsPath]
fromInputDirOrdered config = do
  path <- OsPath.decodeFS $ config.inputDir </> [OsPath.osp|*.tif|]
  pages <- traverse OsPath.encodeFS =<< Glob.glob path
  pure
    $ List.sortBy
      (\a b -> compare (OsPath.takeFileName a) (OsPath.takeFileName b))
    $ filter (OsPath.isExtensionOf [OsPath.osp|tif|]) pages


copyFiles :: Logger -> [PageData OsPath] -> IO ()
copyFiles log [] = log "Finished"
copyFiles log pageData =
  print ()


-- PAPER MATH

toPaperSheetData :: SheetNumber -> SignatureSize -> PaperSheet
toPaperSheetData sheetNumber signatureSize =
  PaperSheet
    (PageNumber frontLeft)
    (PageNumber frontRight)
    (PageNumber backLeft)
    (PageNumber backRight)
 where
  frontLeft = backRight + 1
  frontRight = backLeft - 1
  backLeft = total - backRight + 1
  backRight = (sheetNumber.value - 1) * 2 + 1
  total = signatureSize.value * 4


toSignatureIndex :: SignatureNumber -> SignatureSize -> SignatureIndex
toSignatureIndex signatureNumber signatureSize =
  SignatureIndex $
    foldMap
      ( \sheetNumber ->
          let
            sheetData = toPaperSheetData sheetNumber signatureSize
           in
            Map.fromList
              [ (sheetData.backLeft, (signatureNumber, sheetNumber, BackLeft))
              , (sheetData.backRight, (signatureNumber, sheetNumber, BackRight))
              , (sheetData.frontLeft, (signatureNumber, sheetNumber, FrontLeft))
              , (sheetData.frontRight, (signatureNumber, sheetNumber, FrontRight))
              ]
      )
      ixs
 where
  ixs = take signatureSize.value $ SheetNumber <$> [1 ..]


generateSignatureIndecies :: [SignatureSize] -> [SignatureIndex]
generateSignatureIndecies =
  fmap (uncurry toSignatureIndex) . zip (SignatureNumber <$> [1 ..])


listToSignatureSizes :: PageAmount -> SignatureSize -> [SignatureSize]
listToSignatureSizes (PageAmount 0) _ = []
listToSignatureSizes pageAmount signatureSize =
  if pageAmount.value < optioned
    then [SignatureSize . ceiling $ int2Float pageAmount.value / 4]
    else
      signatureSize
        : listToSignatureSizes (PageAmount (pageAmount.value - optioned)) signatureSize
 where
  optioned = signatureSize.value * 4


listToPosition ::
  (Eq a, Show a) =>
  Offset ->
  SignatureSize ->
  [a] ->
  [PageData a]
listToPosition offset signatureSize xs =
  [ PageData
      { content = content
      , pageNumber = pageNumber
      , signatureNumber = signatureNumber
      , sheetNumber = sheetNumber
      , positionOnSheet = positionOnSheet
      }
  | (content, (pageNumber, (signatureNumber, sheetNumber, positionOnSheet))) <-
      zip
        xs
        ( drop offset.value.value $
            concatMap (Map.toList . (.value)) signatureIndecies
        )
  ]
 where
  pageAmount = PageAmount $ length xs + offset.value.value
  sizes = listToSignatureSizes pageAmount signatureSize
  signatureIndecies = generateSignatureIndecies sizes


-- CLI

data Config
  = Config
  { inputDir :: !OsPath
  , signatureSizeOption :: !SignatureSizeOption
  , offset :: !Offset
  , dryRun :: !Bool
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
    <$> parseInputDir
    <*> parseSignatureSizeOption
    <*> parseOffset
    <*> parseDryRun


parseInputDir :: OptParse.Parser OsPath
parseInputDir =
  OptParse.option (OptParse.maybeReader OsPath.encodeUtf) $
    OptParse.long "input"
      <> OptParse.short 'i'
      <> OptParse.metavar "DIR"


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


parseDryRun :: OptParse.Parser Bool
parseDryRun =
  OptParse.flag False True $
    OptParse.long "dry-run"
      <> OptParse.short 'd'
      <> OptParse.showDefault
      <> OptParse.help "Perform dry-run of imposing"
