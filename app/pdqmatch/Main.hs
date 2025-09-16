module Main (main) where

import Control.Arrow ((>>>))
import Control.Monad (void)
import Data.Char (isSpace)
import Data.List (dropWhileEnd, intercalate)
import Data.Maybe (catMaybes)
import Data.Semigroup ((<>))
import qualified Options.Applicative as Opt
import Options.Applicative ((<**>))
import PdQ (PdQ (..), getPdQ)
import Text.XML.HXT.Core (arrL, constA, deep, getText, runX)

data Options = Options
  { summaryQuery :: Maybe String
  }

optionsParser :: Opt.Parser Options
optionsParser =
  Options
    <$> Opt.optional
      ( Opt.strOption
          ( Opt.short 's'
              <> Opt.long "summary"
              <> Opt.metavar "SUMMARY"
              <> Opt.help "Print PdQ file paths whose summary matches SUMMARY"
          )
      )

optionsInfo :: Opt.ParserInfo Options
optionsInfo =
  Opt.info
    (optionsParser <**> Opt.helper)
    ( Opt.fullDesc
        <> Opt.progDesc "Read PdQ file paths from stdin and optionally match their summaries"
    )

main :: IO ()
main = do
  opts <- Opt.execParser optionsInfo
  contents <- getContents
  let files = map strip (lines contents)
      valid = filter (not . null) files
  case summaryQuery opts of
    Nothing -> mapM_ (void . getPdQ) valid
    Just query -> do
      let normalizedQuery = strip query
      matches <- mapM (matchSummary normalizedQuery) valid
      mapM_ putStrLn (catMaybes matches)

matchSummary :: String -> FilePath -> IO (Maybe FilePath)
matchSummary query path = do
  pdq <- getPdQ path
  mSummary <- extractSummary pdq
  pure $ case mSummary of
    Just summaryText | summaryText == query -> Just path
    _ -> Nothing

extractSummary :: PdQ -> IO (Maybe String)
extractSummary pdq =
  case summary pdq of
    Nothing -> pure Nothing
    Just trees -> do
      fragments <- runX (constA trees >>> arrL id >>> deep getText)
      let trimmed = filter (not . null) (map strip fragments)
          text = strip (intercalate "\n" trimmed)
      if null text then pure Nothing else pure (Just text)

strip :: String -> String
strip = dropWhile isSpace . dropWhileEnd isSpace
