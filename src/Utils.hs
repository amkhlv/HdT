{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Utils (openEditor, openInkscape) where

import System.Directory (getHomeDirectory)
import qualified System.Process as SysProc

openEditor :: String -> IO ()
openEditor filename = do
  home <- getHomeDirectory
  r <- SysProc.createProcess $ SysProc.proc (home ++ "/.config/hdt/edit-pdq.sh") [filename]
  return ()

openInkscape :: String -> IO ()
openInkscape filename = do
  r <- SysProc.createProcess $ SysProc.proc "inkscape" [filename]
  return ()
