{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module PrepSVG (prepSVG) where

import Control.Monad (unless)
import Data.Maybe (isJust)
import Data.Text (isSuffixOf, pack)
import PyF
import System.Directory (createDirectory, doesDirectoryExist, doesFileExist, getCurrentDirectory)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import qualified System.Process as SysProc

createLayer :: String -> String -> IO SysProc.ProcessHandle
createLayer filename layername = do
  (_, _, _, handle) <-
    SysProc.createProcess
      ( SysProc.proc
          "xmlstarlet"
          $ words
            [fmt|
ed --inplace
-N s=http://www.w3.org/2000/svg
-s /s:svg -t attr -n xmlns:inkscape -v http://www.inkscape.org/namespaces/inkscape
-s /s:svg -t elem -n g
--var NEWL $prev
-s $NEWL -t attr -n inkscape:label -v hdt
-s $NEWL -t attr -n inkscape:groupmode -v layer
-s $NEWL -t attr -n id -v {layername}
 |]
            ++ [filename]
      )
  return handle

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
          (_, _, _, pdftocairo) <- SysProc.createProcess $ SysProc.proc "pdftocairo" [pdfFile, svgFile, "-f", show pg, "-l", show pg, "-svg"]
          pdftocairoExitCode <- SysProc.waitForProcess pdftocairo
          unless (pdftocairoExitCode == ExitSuccess) (error "pdftocairo error")
          starlet <- createLayer svgFile layername
          starletExitCode <- SysProc.waitForProcess starlet
          unless (starletExitCode == ExitSuccess) (error "xmlstarlet error")
      return svgFile
    else error "Not in an .hdt directory"
