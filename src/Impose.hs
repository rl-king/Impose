{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Impose where

import Data.List qualified as List
import Data.Map qualified as Map
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


toPaper :: Int -> Int -> Paper
toPaper paper totalPages =
  Paper fl fr bl br
 where
  fl = br + 1
  fr = bl - 1
  bl = totalPages - br + 1
  br = (paper - 1) * 2 + 1


toIndex :: Int -> Map.Map Int Int
toIndex totalPages =
  foldr
    ( \ix acc ->
        let
          paper = toPaper ix totalPages
         in
          Map.fromList
            [ (paper.backLeft, ix)
            , (paper.backRight, ix)
            , (paper.frontLeft, ix)
            , (paper.frontRight, ix)
            ]
            <> acc
    )
    mempty
    (take (ceiling (int2Float totalPages / 4)) [1 ..])


-- CLI

data Config
  = Config
  { inputDir :: !FilePath
  , bundleCount :: !Int
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


parseBundleCount :: Parser Int
parseBundleCount =
  option auto $
    long "bundleCount"
      <> short 'b'
      <> showDefault
      <> value 1
      <> metavar "INT"
      <> help "How many folded bundleCount do you need?"
