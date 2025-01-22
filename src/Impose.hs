{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Impose where

import Data.Foldable (traverse_)
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
  print config
  pages <- fromInputDirOrdered config
  let signatureSize =
        case config.signatureSize of
          Auto -> ceiling $ int2Float (length pages) / 4
          Custom n -> n
      positions = listToPosition config.offset signatureSize pages
  traverse_ print positions


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
  index = toIndex signatureSize
  xsWithIndex = zip xs [offset + 1 ..]


-- CLI

data Config
  = Config
  { inputDir :: !FilePath
  , signatureSize :: !SignatureSize
  , offset :: !Int
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
    <*> parseSignatureSize
    <*> parseOffset


data SignatureSize = Auto | Custom !Int
  deriving (Show, Eq)


parseSignatureSize :: Parser SignatureSize
parseSignatureSize =
  option (fmap Custom auto) $
    long "signatureSize"
      <> short 's'
      <> showDefault
      <> value Auto
      <> metavar "INT"
      <> help "How many papers per signature"


parseOffset :: Parser Int
parseOffset =
  option auto $
    long "offset"
      <> short 'o'
      <> showDefault
      <> value 0
      <> metavar "INT"
      <> help "At what page does the book start"
