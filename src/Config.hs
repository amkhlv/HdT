{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Config
  ( Clr (..),
    HintStyle (..),
    Antialias (..),
    Config (..),
    getConfig,
    getCSSFile,
    writeDefaultConf,
  )
where

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Prettyprint.Doc.Render.Text as Prettyprint.Text
import Dhall (Decoder, Generic, Natural, constructor, double, field, input, inputFile, list, natural, record, string, union, unit)
import Dhall.Marshal.Encode (ToDhall, embed, inject)
import qualified Dhall.Pretty
import qualified GHC.IO.Handle
import qualified GI.Gio.Interfaces.File as GFile
import System.Directory (doesFileExist, getHomeDirectory, makeAbsolute)
import System.IO (IOMode (WriteMode), withFile)

data Clr = Clr {r :: Natural, g :: Natural, b :: Natural} deriving (Generic, Show, Eq)

instance ToDhall Clr

data HintStyle = HintStyleDefault | HintStyleNone | HintStyleSlight | HintStyleMedium | HintStyleFull
  deriving (Show, Eq, Generic)

instance ToDhall HintStyle

hstyle :: Decoder HintStyle
hstyle =
  union
    ( (HintStyleDefault <$ constructor "HintStyleDefault" unit)
        <> (HintStyleNone <$ constructor "HintStyleNone" unit)
        <> (HintStyleSlight <$ constructor "HintStyleSlight" unit)
        <> (HintStyleMedium <$ constructor "HintStyleMedium" unit)
        <> (HintStyleFull <$ constructor "HintStyleFull" unit)
    )

data Antialias = AntialiasDefault | AntialiasNone | AntialiasGray | AntialiasSubpixel | AntialiasFast | AntialiasGood | AntialiasBest
  deriving (Show, Eq, Generic)

instance ToDhall Antialias

antialiasDecoder :: Decoder Antialias
antialiasDecoder =
  union
    ( (AntialiasDefault <$ constructor "AntialiasDefault" unit)
        <> (AntialiasNone <$ constructor "AntialiasNone" unit)
        <> (AntialiasGray <$ constructor "AntialiasGray" unit)
        <> (AntialiasSubpixel <$ constructor "AntialiasSubpixel" unit)
        <> (AntialiasFast <$ constructor "AntialiasFast" unit)
        <> (AntialiasGood <$ constructor "AntialiasGood" unit)
        <> (AntialiasBest <$ constructor "AntialiasBest" unit)
    )

data Config = Config
  { overlayLayerID :: String,
    initialScale :: Double,
    scaleStep :: Double,
    markerSize :: Double,
    markerColors :: [Clr],
    defaultMarkerColor :: Clr,
    dX :: Double,
    dY :: Double,
    windowWidth :: Natural,
    windowHeight :: Natural,
    hintStyle :: HintStyle,
    antialias :: Antialias
  }
  deriving (Generic, Show, Eq)

instance ToDhall Config

clr :: Decoder Clr
clr = record (Clr <$> field "r" natural <*> field "g" natural <*> field "b" natural)

configDecoder :: Decoder Config
configDecoder =
  record
    ( Config
        <$> field "overlayLayerID" string
        <*> field "initialScale" double
        <*> field "scaleStep" double
        <*> field "markerSize" double
        <*> field "markerColors" (list clr)
        <*> field "defaultMarkerColor" clr
        <*> field "dX" double
        <*> field "dY" double
        <*> field "windowWidth" natural
        <*> field "windowHeight" natural
        <*> field "hintStyle" hstyle
        <*> field "antialias" antialiasDecoder
    )

defaultConf :: Config
defaultConf =
  Config
    { overlayLayerID = "layer1",
      initialScale = 1.5,
      scaleStep = 1.05,
      markerSize = 8.0,
      markerColors = [Clr {r = 255, g = 255, b = 0}, Clr {r = 255, b = 0, g = 0}, Clr {r = 0, g = 255, b = 0}, Clr {r = 0, g = 0, b = 255}],
      defaultMarkerColor = Clr {r = 255, g = 255, b = 0},
      dX = 3.0,
      dY = 3.0,
      windowWidth = 800,
      windowHeight = 1055,
      hintStyle = HintStyleDefault,
      antialias = AntialiasDefault
    }

writeDefaultConf :: FilePath -> IO ()
writeDefaultConf filename =
  withFile
    filename
    WriteMode
    ( \h ->
        Prettyprint.Text.hPutDoc
          h
          ( Dhall.Pretty.prettyCharacterSet Dhall.Pretty.Unicode (embed inject (defaultConf :: Config))
          )
    )

getConfig :: String -> IO Config
getConfig dDir = do
  d <- makeAbsolute dDir
  configFileInDDir <- doesFileExist (d ++ "/config.dhall")
  if configFileInDDir
    then do
      putStrLn $ "-- loading extra config file:" ++ d ++ "/config.dhall"
      input configDecoder (T.pack $ "~/.config/HdTPDFViewer/config.dhall // " ++ d ++ "/config.dhall")
    else input configDecoder "~/.config/HdTPDFViewer/config.dhall"

getCSSFile :: String -> IO GFile.File
getCSSFile dDir = do
  d <- makeAbsolute dDir
  cssFileInDDir <- doesFileExist (d ++ "/style.css")
  home <- getHomeDirectory
  GFile.fileNewForPath (if cssFileInDDir then d ++ "/style.css" else home ++ "/.config/HdTPDFViewer/style.css")
