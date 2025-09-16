module Main (main) where

import Data.Semigroup ((<>))
import Options.Applicative
  ( Parser,
    ParserInfo,
    execParser,
    fullDesc,
    help,
    helper,
    info,
    metavar,
    progDesc,
    strArgument,
    (<**>),
  )
import PdQ (PdQ, getPdQ)

newtype Options = Options
  { pdqFile :: FilePath
  }

optionsParser :: Parser Options
optionsParser =
  Options
    <$> strArgument
      ( metavar "PDQ_FILE"
          <> help "Path to the .pdq file"
      )

optionsInfo :: ParserInfo Options
optionsInfo =
  info
    (optionsParser <**> helper)
    ( fullDesc
        <> progDesc "Inspect PdQ files by printing their parsed representation"
    )

run :: Options -> IO ()
run opts = do
  pdq <- getPdQ (pdqFile opts)
  print pdq

main :: IO ()
main = execParser optionsInfo >>= run
