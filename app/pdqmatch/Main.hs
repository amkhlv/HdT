module Main (main) where

import Control.Arrow ((>>>))
import Control.Monad (void)
import Data.Char (isSpace)
import Data.List (any, dropWhileEnd, intercalate, isInfixOf)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Semigroup ((<>))
import Data.Text (pack, stripSuffix, unpack)
import Options.Applicative ((<**>))
import qualified Options.Applicative as Opt
import PdQ (Bookmark (..), Note (..), PdQ (..), getPdQ)
import Text.XML.HXT.Core (arrL, constA, deep, getText, runX)

data Options = Options
  { summaryQuery :: Maybe String,
    tagQuery :: Maybe String,
    anyQuery :: Maybe String,
    printPDF :: Bool
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
    <*> Opt.optional
      ( Opt.strOption
          ( Opt.short 'a'
              <> Opt.long "any"
              <> Opt.metavar "ANY"
              <> Opt.help "Print PdQ file paths whose summary, bookmarks, or notes match ANY"
          )
      )
    <*> Opt.switch (Opt.short 'f' <> Opt.long "pdf" <> Opt.help "Print path to .pdf, instead of .pdq")

optionsInfo :: Opt.ParserInfo Options
optionsInfo =
  Opt.info
    (optionsParser <**> Opt.helper)
    ( Opt.fullDesc
        <> Opt.progDesc "Read PdQ file paths from stdin and optionally match their summaries"
    )

suffixPDF :: String -> String
suffixPDF x = let txt = pack x in unpack (fromMaybe txt $ stripSuffix (pack ".pdq") txt) ++ ".pdf"

main :: IO ()
main = do
  opts <- Opt.execParser optionsInfo
  contents <- getContents
  let files = map strip (lines contents)
      valid = filter (not . null) files
      normalizedSummaryQuery = strip <$> summaryQuery opts
      normalizedTagQuery = strip <$> tagQuery opts
      normalizedAnyQuery = strip <$> anyQuery opts
  case (normalizedSummaryQuery, normalizedTagQuery, normalizedAnyQuery) of
    (Nothing, Nothing, Nothing) -> mapM_ getPdQ valid
    _ -> do
      matches <- mapM (matchFilters normalizedSummaryQuery normalizedTagQuery normalizedAnyQuery) valid
      mapM_ (putStrLn . if printPDF opts then suffixPDF else id) (catMaybes matches)

matchFilters :: Maybe String -> Maybe String -> Maybe String -> FilePath -> IO (Maybe FilePath)
matchFilters mSummaryQuery mTagQuery mAnyQuery path = do
  pdq <- getPdQ path
  let needsSummary = maybe False (const True) mSummaryQuery || maybe False (const True) mAnyQuery
  mSummary <- if needsSummary then extractSummary pdq else pure Nothing
  let summaryMatches =
        case mSummaryQuery of
          Nothing -> Nothing
          Just query -> Just $ maybe False (isInfixOf query) mSummary
      tagMatches = case mTagQuery of
        Nothing -> Nothing
        Just tag -> Just $ maybe False (elem tag . map strip) (tags pdq)
      anyMatches =
        case mAnyQuery of
          Nothing -> Nothing
          Just query ->
            let summaryMatch = maybe False (isInfixOf query) mSummary
                bookmarkMatches =
                  maybe
                    False
                    (any (\b -> isInfixOf query (strip (title b))))
                    (bookmarks pdq)
                noteMatches =
                  maybe
                    False
                    (any (\n -> maybe False (isInfixOf query) (strip <$> note n)))
                    (notes pdq)
             in Just (summaryMatch || bookmarkMatches || noteMatches)
      activeMatches = catMaybes [summaryMatches, tagMatches, anyMatches]
      matches = null activeMatches || or activeMatches
  pure $ if matches then Just path else Nothing

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
