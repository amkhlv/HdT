{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module PrepSVG (prepSVG) where

import Control.Monad (unless, when)
import Data.Maybe (isJust)
import Data.Text (isSuffixOf, pack)
import qualified Data.Text.Lazy as DTL
import Data.Text.Lazy.Encoding (encodeUtf8)
import PyF
import Shh
import System.Directory (createDirectory, doesDirectoryExist, doesFileExist, getCurrentDirectory)
import System.FilePath ((</>))

createLayer :: String -> String -> Proc ()
createLayer filename layername =
  mkProc "xmlstarlet" $
    encodeUtf8 . DTL.pack
      <$> words
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
          statusTrace <- tryFailure $ mkProc "pdftocairo" $ encodeUtf8 . DTL.pack <$> [pdfFile, svgFile, "-f", show pg, "-l", show pg, "-svg"]
          -- statusTrace <- tryFailure $ mkProc "pdf2svg" $ encodeUtf8 . DTL.pack <$> [pdfFile, svgFile, show pg]
          print statusTrace
          statusNewLayer <-
            tryFailure $ createLayer svgFile layername
          print statusNewLayer
      return svgFile
    else error "Not in an .hdt directory"
