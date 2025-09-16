module Main (main) where

import Control.Arrow ((>>>))
import Control.Monad (void)
import Data.Char (isSpace)
import Data.List (dropWhileEnd, intercalate, isInfixOf)
import Data.Maybe (catMaybes)
import Data.Semigroup ((<>))
import qualified Options.Applicative as Opt
import Options.Applicative ((<**>))
import PdQ (PdQ (..), getPdQ)
import Text.XML.HXT.Core (arrL, constA, deep, getText, runX)

data Options = Options
  { summaryQuery :: Maybe String,
    tagQuery :: Maybe String
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
    <*> Opt.optional
      ( Opt.strOption
          ( Opt.short 't'
              <> Opt.long "tag"
              <> Opt.metavar "TAG"
              <> Opt.help "Print PdQ file paths whose tag exactly matches TAG"
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
      normalizedSummaryQuery = strip <$> summaryQuery opts
      normalizedTagQuery = strip <$> tagQuery opts
  case (normalizedSummaryQuery, normalizedTagQuery) of
    (Nothing, Nothing) -> mapM_ (void . getPdQ) valid
    _ -> do
      matches <- mapM (matchFilters normalizedSummaryQuery normalizedTagQuery) valid
      mapM_ putStrLn (catMaybes matches)

matchFilters :: Maybe String -> Maybe String -> FilePath -> IO (Maybe FilePath)
matchFilters mSummaryQuery mTagQuery path = do
  pdq <- getPdQ path
  summaryMatches <- case mSummaryQuery of
    Nothing -> pure True
    Just query -> do
      mSummary <- extractSummary pdq
      pure $ maybe False (query `isInfixOf`) mSummary
  let tagMatches = case mTagQuery of
        Nothing -> True
        Just tag -> maybe False (elem tag . map strip) (tags pdq)
  pure $ if summaryMatches && tagMatches then Just path else Nothing

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
