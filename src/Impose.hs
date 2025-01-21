{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Impose where

import Data.Bifunctor (bimap)
import Data.List qualified as List
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
  print $ reorder pages


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


paper :: Paper
paper =
  Paper 2 15 1 16


paperToPages :: Int -> Int -> Paper
paperToPages paper totalPages =
  Paper fl fr bl br
 where
  fl = br + 1
  fr = br - 1
  bl = 1
  br = 1 + (paper - 1) * 2


reorder :: [a] -> [a]
reorder list =
  mconcat $ zipWith (\a b -> [a, b]) odd even
 where
  (odd, even) = reverse <$> oddEven list


oddEven :: [a] -> ([a], [a])
oddEven [] = ([], [])
oddEven [x] = ([x], [])
oddEven (x : y : xs) = bimap (x :) (y :) (oddEven xs)


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
