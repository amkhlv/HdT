module Main (main) where

import Control.Monad (void)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import PdQ (getPdQ)

main :: IO ()
main = do
  contents <- getContents
  let files = map strip (lines contents)
      valid = filter (not . null) files
  mapM_ (void . getPdQ) valid

strip :: String -> String
strip = dropWhile isSpace . dropWhileEnd isSpace
