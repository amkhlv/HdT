{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module PrepSVG (prepSVG) where

import Control.Monad (unless)
import Data.Maybe (isJust)
import Data.Text (isSuffixOf, pack)
import GI.Gio.Objects.Cancellable
import qualified GI.Gio.Objects.Subprocess as GSub
import System.Directory (createDirectory, doesDirectoryExist, doesFileExist, getCurrentDirectory, getHomeDirectory)
import System.FilePath ((</>))

prepSVG :: Int -> String -> Maybe String -> IO String
prepSVG pg layername mdDir = do
  wd <- maybe getCurrentDirectory return mdDir
  if isSuffixOf ".hdt" $ pack wd
    then do
      existsDir <- doesDirectoryExist wd
      unless existsDir (createDirectory wd)
      let pdfFile = (init . init . init) wd ++ "pdf"
      let svgFile0 = "p" ++ show pg ++ ".svg"
      let svgFile = if isJust mdDir then wd </> svgFile0 else svgFile0
      alreadyTraced <- doesFileExist svgFile
      if alreadyTraced
        then putStrLn $ "Using already existing SVG file " ++ svgFile
        else do
          home <- getHomeDirectory
          sub <- GSub.subprocessNew ["sh", home ++ "/.config/HdTPDFViewer/pdf-to-svg.sh", pdfFile, svgFile, show pg] []
          GSub.subprocessWait sub (Nothing :: Maybe Cancellable)
          sub1 <- GSub.subprocessNew ["sh", home ++ "/.config/HdTPDFViewer/insert-layer.sh", layername, svgFile] []
          GSub.subprocessWait sub1 (Nothing :: Maybe Cancellable)
      return svgFile
    else error "Not in an .hdt directory"
