{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Config
  ( Config (..),
    getConfig,
  )
where

import qualified Data.Map as M
import qualified Data.Text as T
import Dhall (Decoder, Generic, Natural, double, field, inputFile, list, natural, record, string)
import System.Directory (getHomeDirectory)

data Config = Config
  { overlayLayerID :: String,
    initialScale :: Double,
    scaleStep :: Double,
    markerSize :: Double,
    dX :: Double,
    dY :: Double,
    windowWidth :: Natural,
    windowHeight :: Natural
  }
  deriving (Generic, Show, Eq)

configDecoder :: Decoder Config
configDecoder =
  record
    ( Config
        <$> field "overlayLayerID" string
        <*> field "initialScale" double
        <*> field "scaleStep" double
        <*> field "markerSize" double
        <*> field "dX" double
        <*> field "dY" double
        <*> field "windowWidth" natural
        <*> field "windowHeight" natural
    )

getConfig :: IO Config
getConfig = do
  home <- getHomeDirectory
  inputFile configDecoder (home ++ "/.config/hdt.dhall")
