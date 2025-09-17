{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use maybe" #-}
module Main (main) where

import Control.Arrow ((>>>))
import Control.Monad (unless, when)
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
    some,
    strArgument,
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

data Section = Path | Summary | Tags | Bookmarks | Notes deriving (Show, Eq)

data Options = Options
  { pdqFiles :: [FilePath],
    showSummary :: Bool,
    brief :: Bool,
    showTags :: Bool
  }

optionsParser :: Parser Options
optionsParser =
  Options
    <$> some
      ( strArgument
          ( metavar "PdQ_FILE..."
              <> help "Paths to the .pdq files"
          )
      )
    <*> switch
      ( short 's'
          <> long "summary"
          <> help "Print the PdQ summary"
      )
    <*> switch
      ( short 'b'
          <> long "brief"
          <> help "Only print file path and summary"
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
    anySelector = or [showSummary opts, showTags opts]
    process path = do
      pdq <- getPdQ path
      if not anySelector
        then printDefault (brief opts) path pdq
        else do
          when (showSummary opts) $ printSummary pdq
          when (showTags opts) $ printTags pdq

main :: IO ()
main = execParser optionsInfo >>= run

printDefault :: Bool -> FilePath -> PdQ -> IO ()
printDefault beBrief path pdq = do
  let bookmarksSection = bookmarkLines pdq
      notesSection = noteLines pdq
  putStrLn $ green path
  printSummary pdq
  unless beBrief (printSections $ filter (not . null . snd) [(Bookmarks, bookmarksSection), (Notes, notesSection)])

bold :: String -> String
bold text = setSGRCode [SetConsoleIntensity BoldIntensity] ++ text ++ setSGRCode [Reset]

green :: String -> String
green text =
  setSGRCode [SetColor Foreground Vivid Green] ++ text ++ setSGRCode [Reset]

blue :: String -> String
blue text =
  setSGRCode [SetColor Foreground Vivid Blue, SetConsoleIntensity BoldIntensity] ++ text ++ setSGRCode [Reset]

printSections :: [(Section, [String])] -> IO ()
printSections [] = pure ()
printSections [section] = printSection section >> putStrLn ""
printSections (section : rest) = do
  printSection section
  printSections rest

printSection :: (Section, [String]) -> IO ()
printSection (ttl, contents) = do
  putStrLn $ blue $ "  " ++ show ttl
  putStr " "
  mapM_ (putStr . (++) " • ") contents

printSummary :: PdQ -> IO ()
printSummary pdq = do
  fragments <- summaryLines pdq
  unless
    (null fragments)
    (putStr " " >> mapM_ (putStr . bold . (" " ++)) fragments >> putStrLn "")

summaryLines :: PdQ -> IO [String]
summaryLines pdq =
  case summary pdq of
    Nothing -> pure []
    Just trees -> do
      fragments <- runX (constA trees >>> arrL id >>> deep getText)
      pure (filter (any (not . isSpace)) fragments)

noteLines :: PdQ -> [String]
noteLines pdq = filter (any (not . isSpace)) . mapMaybe note $ fromMaybe [] (notes pdq)

bookmarkLines :: PdQ -> [String]
bookmarkLines pdq =
  map
    (\b -> title b ++ " (page " ++ show (bookmarkPage b) ++ ")\n")
    (fromMaybe [] (bookmarks pdq))

printTags :: PdQ -> IO ()
printTags pdq =
  mapM_ putStrLn $ fromMaybe [] (tags pdq)
