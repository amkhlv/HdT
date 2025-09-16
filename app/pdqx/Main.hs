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
    some,
    switch,
    (<**>),
  )
import PdQ (Bookmark (..), Note (..), PdQ (..), getPdQ)
import System.Console.ANSI
  ( Color (Blue, Green, Yellow),
    ColorIntensity (Vivid),
    ConsoleIntensity (BoldIntensity),
    ConsoleLayer (Foreground),
    SGR (Reset, SetColor, SetConsoleIntensity),
    setSGRCode,
  )
import Text.XML.HXT.Core (arrL, constA, deep, getText, runX)

data Options = Options
  { pdqFiles :: [FilePath],
    showSummary :: Bool,
    showNotes :: Bool,
    showBookmarks :: Bool,
    showTags :: Bool
  }

optionsParser :: Parser Options
optionsParser =
  Options
    <$> some
      ( strArgument
          ( metavar "PDQ_FILE..."
              <> help "Paths to the .pdq files"
          )
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
run opts = mapM_ process (pdqFiles opts)
  where
    anySelector = or [showSummary opts, showNotes opts, showBookmarks opts, showTags opts]
    process path = do
      pdq <- getPdQ path
      if not anySelector
        then printDefault path pdq
        else do
          when (showSummary opts) $ printSummary pdq
          when (showNotes opts) $ printNotes pdq
          when (showBookmarks opts) $ printBookmarks pdq
          when (showTags opts) $ printTags pdq

main :: IO ()
main = execParser optionsInfo >>= run

printDefault :: FilePath -> PdQ -> IO ()
printDefault path pdq = do
  summarySection <- summaryLines pdq
  let bookmarksSection = bookmarkLines pdq
      notesSection = noteLines pdq
      sections =
        filter
          (not . null . snd)
          [ ("Path", [path]),
            ("Summary", summarySection),
            ("Bookmarks", bookmarksSection),
            ("Notes", notesSection)
          ]
      highlighted = map highlight sections
  printSections highlighted
  where
    highlight section@(title, contents)
      | title == "Path" = (title, map green contents)
      | title == "Summary" = (title, map bold contents)
      | otherwise = section

bold :: String -> String
bold text = setSGRCode [SetConsoleIntensity BoldIntensity] ++ text ++ setSGRCode [Reset]

green :: String -> String
green text =
  setSGRCode [SetColor Foreground Vivid Green] ++ text ++ setSGRCode [Reset]

blue :: String -> String
blue text =
  setSGRCode [SetColor Foreground Vivid Blue, SetConsoleIntensity BoldIntensity] ++ text ++ setSGRCode [Reset]

printSections :: [(String, [String])] -> IO ()
printSections [] = pure ()
printSections [section] = printSection section >> putStrLn ""
printSections (section : rest) = do
  printSection section
  printSections rest

printSection :: (String, [String]) -> IO ()
printSection (title, contents) = do
  when (title == "Bookmarks" || title == "Notes") (putStrLn $ blue title)
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
