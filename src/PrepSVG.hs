{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module PrepSVG (prepSVG) where

import Data.Text (isSuffixOf, pack)
import qualified Data.Text.Lazy as DTL
import Data.Text.Lazy.Encoding (encodeUtf8)
import PyF
import Shh
import System.Directory (doesFileExist, getCurrentDirectory)

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

prepSVG :: Int -> String -> IO ()
prepSVG pg layername = do
  wd <- getCurrentDirectory
  if isSuffixOf ".d" $ pack wd
    then do
      let pdfFile = init wd ++ "pdf"
      let svgFile = "p" ++ show pg ++ ".svg"
      alreadyTraced <- doesFileExist svgFile
      if alreadyTraced
        then putStrLn "Using already existing pdf file"
        else do
          statusTrace <- tryFailure $ mkProc "pdftocairo" $ encodeUtf8 . DTL.pack <$> [pdfFile, svgFile, "-f", show pg, "-l", show pg, "-svg"]
          -- statusTrace <- tryFailure $ mkProc "pdf2svg" $ encodeUtf8 . DTL.pack <$> [pdfFile, svgFile, show pg]
          print statusTrace
          statusNewLayer <-
            tryFailure $ createLayer svgFile layername
          print statusNewLayer
    else error "Not in a pdfFile.d directory"
