{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Config
  ( Clr (..),
    Config (..),
    getConfig,
    getCSSFile,
  )
where

import qualified Data.Map as M
import qualified Data.Text as T
import Dhall (Decoder, Generic, Natural, double, field, input, inputFile, list, natural, record, string)
import qualified GI.Gio.Interfaces.File as GFile
import System.Directory (doesFileExist, getHomeDirectory, makeAbsolute)

data Clr = Clr {r :: Natural, g :: Natural, b :: Natural} deriving (Generic, Show, Eq)

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
    windowHeight :: Natural
  }
  deriving (Generic, Show, Eq)

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
    )

getConfig :: String -> IO Config
getConfig dDir = do
  d <- makeAbsolute dDir
  configFileInDDir <- doesFileExist (d ++ "/config.dhall")
  if configFileInDDir
    then do
      putStrLn $ "-- loading extra config file:" ++ d ++ "/config.dhall"
      input configDecoder (T.pack $ "~/.config/hdt/config.dhall // " ++ d ++ "/config.dhall")
    else input configDecoder "~/.config/hdt/config.dhall"

getCSSFile :: String -> IO GFile.File
getCSSFile dDir = do
  d <- makeAbsolute dDir
  cssFileInDDir <- doesFileExist (d ++ "/style.css")
  home <- getHomeDirectory
  GFile.fileNewForPath (if cssFileInDDir then d ++ "/style.css" else home ++ "/.config/hdt/style.css")
