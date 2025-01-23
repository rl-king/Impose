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
        case config.signatureSize of
          Auto -> PaperCount $ ceiling $ int2Float (length pages) / 4
          Custom n -> n
      positions = listToPosition config.offset signatureSize pages
  traverse_ print positions


-- DEFS

data Paper
  = Paper
  { frontLeft :: !Int
  , frontRight :: !Int
  , backLeft :: !Int
  , backRight :: !Int
  }
  deriving (Show, Eq)


data PositionOnPaper
  = FrontLeft
  | FrontRight
  | BackLeft
  | BackRight
  deriving (Show, Eq)


newtype PaperCount
  = PaperCount {value :: Int}
  deriving (Show, Eq)


newtype PaperIndex
  = PaperIndex {value :: Int}
  deriving (Show, Eq)


newtype PageCount
  = PageCount {value :: Int}
  deriving (Show, Eq)


newtype SignatureIndex
  = SignatureIndex {value :: Map.Map Int (Int, PositionOnPaper)}
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


toPaper :: PaperIndex -> PaperCount -> Paper
toPaper paperIndex paperCount =
  Paper frontLeft frontRight backLeft backRight
 where
  frontLeft = backRight + 1
  frontRight = backLeft - 1
  backLeft = total - backRight + 1
  backRight = (paperIndex.value - 1) * 2 + 1
  total = paperCount.value * 4


toSignatureIndex :: PaperCount -> SignatureIndex
toSignatureIndex paperCount =
  SignatureIndex $
    foldr
      ( \paper acc ->
          let
            paperPos = toPaper paper paperCount
           in
            Map.fromList
              [ (paperPos.backLeft, (paper.value, BackLeft))
              , (paperPos.backRight, (paper.value, BackRight))
              , (paperPos.frontLeft, (paper.value, FrontLeft))
              , (paperPos.frontRight, (paper.value, FrontRight))
              ]
              <> acc
      )
      mempty
      ixs
 where
  ixs = take paperCount.value $ PaperIndex <$> [1 ..]


toPaperCountList :: [a] -> PaperCount -> [PaperCount]
toPaperCountList xs paperCount =
  PaperCount . length <$> List.Split.chunksOf paperCount.value xs


generateSignatureIndecies :: [PaperCount] -> SignatureIndex
generateSignatureIndecies =
  foldMap toSignatureIndex


listToPosition :: Int -> PaperCount -> [FilePath] -> [(FilePath, Maybe (Int, PositionOnPaper))]
listToPosition offset paperCount xs =
  [(x, Map.lookup ix index.value) | (x, ix) <- xsWithIndex]
 where
  index = toSignatureIndex paperCount
  xsWithIndex = zip xs [offset + 1 ..]


-- CLI

data Config
  = Config
  { inputDir :: !FilePath
  , signatureSize :: !SignatureSize
  , offset :: !Int
  }
  deriving (Show, Eq)


data SignatureSize = Auto | Custom !PaperCount
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
    <*> parseSignatureSize
    <*> parseOffset


parseSignatureSize :: OptParse.Parser SignatureSize
parseSignatureSize =
  OptParse.option (fmap (Custom . PaperCount) OptParse.auto) $
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
