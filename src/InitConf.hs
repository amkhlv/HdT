{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module InitConf (initConf) where

import Config
import Control.Monad (unless)
import Data.Maybe (isJust)
import Data.Text (isSuffixOf, pack)
import GI.Gio.Objects.Cancellable
import qualified GI.Gio.Objects.Subprocess as GSub
import PyF
import System.Directory (createDirectory, doesDirectoryExist, doesFileExist, getCurrentDirectory, getHomeDirectory, makeAbsolute)
import System.FilePath ((</>))
import System.IO (IOMode (WriteMode), hPutStrLn, withFile)

defaultCSS :: String
defaultCSS = ".main-area { background-color: #fff7f0; }"

defaultEditPdQ :: String
defaultEditPdQ = [fmt|
#!/bin/sh

gvim "$1"
 |]

initConf :: IO ()
initConf = do
  home <- getHomeDirectory
  let confDir = home </> ".config" </> "HdTPDFViewer"
  alreadyExists <- doesDirectoryExist confDir
  unless alreadyExists $ do
    createDirectory confDir
    writeDefaultConf $ confDir </> "config.dhall"
    writeFile (confDir </> "style.css") defaultCSS
    writeFile (confDir </> "edit-pdq.sh") defaultEditPdQ
