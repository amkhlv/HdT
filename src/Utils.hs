{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Utils (openEditor, openInkscape) where

import Data.Text (isSuffixOf, pack)
import qualified Data.Text.Lazy as DTL
import Data.Text.Lazy.Encoding (encodeUtf8)
import qualified GI.Gio.Objects.Task as Task
import PyF
import Shh
import System.Directory (doesFileExist, getCurrentDirectory, getHomeDirectory)

openEditor :: String -> IO ()
openEditor filename = do
  home <- getHomeDirectory
  runProc $ mkProc (encodeUtf8 . DTL.pack $ home ++ "/.config/hdt/edit-pdq.sh") [encodeUtf8 $ DTL.pack filename]

openInkscape :: String -> IO ()
openInkscape filename = do
  runProc $ mkProc "inkscape" [encodeUtf8 $ DTL.pack filename]
