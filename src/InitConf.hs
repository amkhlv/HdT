{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module InitConf (initConf) where

import Config
import Control.Monad (unless)
import qualified Data.ByteString as BS
import Data.FileEmbed (embedFileRelative)
import System.Directory (createDirectory, doesDirectoryExist, getHomeDirectory)
import System.FilePath ((</>))

defaultCSS :: BS.ByteString
defaultCSS = $(embedFileRelative "src/config/style.css")

defaultEditPdQ :: BS.ByteString
defaultEditPdQ = $(embedFileRelative "src/config/edit-pdq.sh")

defaultInsertLayer :: BS.ByteString
defaultInsertLayer = $(embedFileRelative "src/config/insert-layer.sh")

defaultPDFtoSVG :: BS.ByteString
defaultPDFtoSVG = $(embedFileRelative "src/config/pdf-to-svg.sh")

initConf :: IO ()
initConf = do
  home <- getHomeDirectory
  let confDir = home </> ".config" </> "HdTPDFViewer"
  alreadyExists <- doesDirectoryExist confDir
  unless alreadyExists $ do
    createDirectory confDir
    writeDefaultConf $ confDir </> "config.dhall"
    BS.writeFile (confDir </> "style.css") defaultCSS
    BS.writeFile (confDir </> "edit-pdq.sh") defaultEditPdQ
    BS.writeFile (confDir </> "pdf-to-svg.sh") defaultPDFtoSVG
    BS.writeFile (confDir </> "insert-layer.sh") defaultInsertLayer
