{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Utils (openEditor, openInkscape) where

import Control.Monad (unless)
import System.Directory (getHomeDirectory)
import System.Exit (ExitCode (ExitSuccess))
import qualified System.Process as SysProc

openEditor :: String -> IO ()
openEditor filename = do
  home <- getHomeDirectory
  (_, _, _, handle) <- SysProc.createProcess $ SysProc.proc (home ++ "/.config/hdt/edit-pdq.sh") [filename]
  code <- SysProc.waitForProcess handle
  unless (code == ExitSuccess) $ print code

openInkscape :: String -> IO ()
openInkscape filename = do
  (_, _, _, handle) <- SysProc.createProcess $ SysProc.proc "inkscape" [filename]
  code <- SysProc.waitForProcess handle
  unless (code == ExitSuccess) $ print code
