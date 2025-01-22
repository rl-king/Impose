{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Impose where

import Data.List qualified as List
import Data.Map qualified as Map
import Debug.Trace qualified as Debug
import GHC.Float (int2Float)
import Options.Applicative
import System.FilePath ((</>))
import System.FilePath qualified as FilePath
import System.FilePath.Glob qualified as Glob


main :: IO ()
main = do
  config <- parseArgs
  pages <- fromInputDirOrdered config
  print config
  print pages


fromInputDirOrdered :: Config -> IO [FilePath]
fromInputDirOrdered config = do
  pages <- Glob.glob $ config.inputDir </> "*.tif"
  pure
    $ List.sortBy
      (\a b -> compare (FilePath.takeFileName a) (FilePath.takeFileName b))
    $ filter (FilePath.isExtensionOf "tif") pages


data Paper
  = Paper
  { frontLeft :: !Int
  , frontRight :: !Int
  , backLeft :: !Int
  , backRight :: !Int
  }
  deriving (Show, Eq)


data PaperPosition
  = FrontLeft
  | FrontRight
  | BackLeft
  | BackRight
  deriving (Show, Eq)


toPaper :: Int -> Int -> Paper
toPaper paper totalPapers =
  Paper fl fr bl br
 where
  fl = br + 1
  fr = bl - 1
  bl = total - br + 1
  br = (paper - 1) * 2 + 1
  total = totalPapers * 4


toIndex :: Int -> Map.Map Int (Int, PaperPosition)
toIndex totalPapers =
  foldr
    ( \paper acc ->
        let
          paperPos = toPaper paper totalPapers
         in
          Map.fromList
            [ (paperPos.backLeft, (paper, BackLeft))
            , (paperPos.backRight, (paper, BackRight))
            , (paperPos.frontLeft, (paper, FrontLeft))
            , (paperPos.frontRight, (paper, FrontRight))
            ]
            <> acc
    )
    mempty
    ixs
 where
  ixs = take totalPapers [1 ..]


listToPosition :: Int -> Int -> [FilePath] -> [(FilePath, Maybe (Int, PaperPosition))]
listToPosition offset signatureSize xs =
  [(x, Map.lookup ix index) | (x, ix) <- xsWithIndex]
 where
  index = Debug.traceShowId $ toIndex signatureSize
  xsWithIndex = zip xs [offset + 1 ..]


-- CLI

data Config
  = Config
  { inputDir :: !FilePath
  , signatureSize :: !Int
  , firstPage :: !Int
  }
  deriving (Show, Eq)


parseArgs :: IO Config
parseArgs =
  customExecParser (prefs showHelpOnError) $
    info (parser <**> helper) (fullDesc <> progDesc "impose")


parser :: Parser Config
parser =
  Config
    <$> strOption
      ( long "input"
          <> short 'i'
          <> metavar "DIR"
      )
    <*> parseBundleCount
    <*> parseFirstPage


parseBundleCount :: Parser Int
parseBundleCount =
  option auto $
    long "signatureSize"
      <> short 's'
      <> showDefault
      <> value 1
      <> metavar "INT"
      <> help "How many papers per signature"


parseFirstPage :: Parser Int
parseFirstPage =
  option auto $
    long "firstPage"
      <> short 'f'
      <> showDefault
      <> value 1
      <> metavar "INT"
      <> help "At what page does the book start"
