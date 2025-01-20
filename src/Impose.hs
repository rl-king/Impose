{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Impose where

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


reorder :: [a] -> ([a], [a])
reorder list =
  (odd, even)
 where
  (odd, even) = oddEven list
  oddEven [] = ([], [])
  oddEven (x : []) = ([x], [])
  oddEven (x : y : xs) = (x : (fst $ oddEven xs), y : (snd $ oddEven xs))


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
