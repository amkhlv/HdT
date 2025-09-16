module Main (main) where

import Control.Arrow ((>>>))
import Control.Monad (when)
import Data.Char (isSpace)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Semigroup ((<>))
import Options.Applicative
  ( Parser,
    ParserInfo,
    execParser,
    fullDesc,
    help,
    helper,
    info,
    long,
    metavar,
    progDesc,
    short,
    strArgument,
    switch,
    (<**>),
  )
import PdQ (Bookmark (..), Note (..), PdQ (..), getPdQ)
import Text.XML.HXT.Core (arrL, constA, deep, getText, runX)

data Options = Options
  { pdqFile :: FilePath,
    showSummary :: Bool,
    showNotes :: Bool,
    showBookmarks :: Bool,
    showTags :: Bool
  }

optionsParser :: Parser Options
optionsParser =
  Options
    <$> strArgument
      ( metavar "PDQ_FILE"
          <> help "Path to the .pdq file"
      )
    <*> switch
      ( short 's'
          <> long "summary"
          <> help "Print the PdQ summary"
      )
    <*> switch
      ( short 'n'
          <> long "notes"
          <> help "Print PdQ notes"
      )
    <*> switch
      ( short 'b'
          <> long "bookmarks"
          <> help "Print PdQ bookmarks"
      )
    <*> switch
      ( short 't'
          <> long "tags"
          <> help "Print PdQ tags"
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
  let anySelector = or [showSummary opts, showNotes opts, showBookmarks opts, showTags opts]
  if not anySelector
    then print pdq
    else do
      when (showSummary opts) $ printSummary pdq
      when (showNotes opts) $ printNotes pdq
      when (showBookmarks opts) $ printBookmarks pdq
      when (showTags opts) $ printTags pdq

main :: IO ()
main = execParser optionsInfo >>= run

printSummary :: PdQ -> IO ()
printSummary pdq =
  case summary pdq of
    Nothing -> pure ()
    Just trees -> do
      fragments <- runX (constA trees >>> arrL id >>> deep getText)
      mapM_ putStrLn (filter (any (not . isSpace)) fragments)

printNotes :: PdQ -> IO ()
printNotes pdq =
  mapM_ putStrLn . filter (any (not . isSpace)) . mapMaybe note $ fromMaybe [] (notes pdq)

printBookmarks :: PdQ -> IO ()
printBookmarks pdq =
  mapM_ (\b -> putStrLn $ title b ++ " (page " ++ show (bookmarkPage b) ++ ")") $ fromMaybe [] (bookmarks pdq)

printTags :: PdQ -> IO ()
printTags pdq =
  mapM_ putStrLn $ fromMaybe [] (tags pdq)
