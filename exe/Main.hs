module Main where

import Options.Applicative


main :: IO ()
main = do
  config <- parseArgs
  print config
  putStrLn "Hello, Haskell!"


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
