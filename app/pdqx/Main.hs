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
import System.Console.ANSI
  ( ConsoleIntensity (BoldIntensity),
    SGR (Reset, SetConsoleIntensity),
    setSGRCode
  )
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
    then printDefault opts pdq
    else do
      when (showSummary opts) $ printSummary pdq
      when (showNotes opts) $ printNotes pdq
      when (showBookmarks opts) $ printBookmarks pdq
      when (showTags opts) $ printTags pdq

main :: IO ()
main = execParser optionsInfo >>= run

printDefault :: Options -> PdQ -> IO ()
printDefault opts pdq = do
  summarySection <- summaryLines pdq
  let bookmarksSection = bookmarkLines pdq
      notesSection = noteLines pdq
      sections =
        filter (not . null . snd)
          [ ("Path", [pdqFile opts]),
            ("Summary", summarySection),
            ("Bookmarks", bookmarksSection),
            ("Notes", notesSection)
          ]
      highlighted = map highlight sections
  printSections highlighted
  where
    highlight section@(title, contents)
      | title `elem` ["Path", "Summary"] = (title, map bold contents)
      | otherwise = section

bold :: String -> String
bold text = setSGRCode [SetConsoleIntensity BoldIntensity] ++ text ++ setSGRCode [Reset]

printSections :: [(String, [String])] -> IO ()
printSections [] = pure ()
printSections [section] = printSection section
printSections (section : rest) = do
  printSection section
  putStrLn ""
  printSections rest

printSection :: (String, [String]) -> IO ()
printSection (title, contents) = do
  putStrLn title
  mapM_ putStrLn contents

printSummary :: PdQ -> IO ()
printSummary pdq = do
  fragments <- summaryLines pdq
  mapM_ putStrLn fragments

summaryLines :: PdQ -> IO [String]
summaryLines pdq =
  case summary pdq of
    Nothing -> pure []
    Just trees -> do
      fragments <- runX (constA trees >>> arrL id >>> deep getText)
      pure (filter (any (not . isSpace)) fragments)

printNotes :: PdQ -> IO ()
printNotes pdq =
  mapM_ putStrLn (noteLines pdq)

noteLines :: PdQ -> [String]
noteLines pdq = filter (any (not . isSpace)) . mapMaybe note $ fromMaybe [] (notes pdq)

printBookmarks :: PdQ -> IO ()
printBookmarks pdq =
  mapM_ putStrLn (bookmarkLines pdq)

bookmarkLines :: PdQ -> [String]
bookmarkLines pdq =
  map
    (\b -> title b ++ " (page " ++ show (bookmarkPage b) ++ ")")
    (fromMaybe [] (bookmarks pdq))

printTags :: PdQ -> IO ()
printTags pdq =
  mapM_ putStrLn $ fromMaybe [] (tags pdq)
