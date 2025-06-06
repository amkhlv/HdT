{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Utils (openInkscape, editPdQ) where

import qualified GI.Gio.Objects.Subprocess as GSub
import System.Directory (getHomeDirectory)

editPdQ :: String -> IO ()
editPdQ filename = do
  home <- getHomeDirectory
  _ <- GSub.subprocessNew ["sh", home ++ "/.config/hdt/edit-pdq.sh", filename] []
  return ()

openInkscape :: String -> IO ()
openInkscape filename = do
  _ <- GSub.subprocessNew ["inkscape", filename] []
  putStrLn "-- returning from Inkscape"
