{-# LANGUAGE OverloadedStrings #-}

module CDP.Tutorial
  ( main
  ) where

import           Control.Monad        (void)
import           Data.Foldable        (traverse_)
import           Safe                 (headMay)

import           Control.Monad.Trans  (liftIO)

import           CDP.Target
import           CDP.Target.Async  (onEvent, stopEventListener, waitFor)
import           CDP.Target.Client

import qualified CDP.API.Network   as Network
import qualified CDP.API.Page      as Page

main :: IO ()
main = fetchTarget >>= maybe onPageNotFound runCommands
  where
    fetchTarget = (headMay =<<) <$> fetchTargets "http://localhost:9222"
    onPageNotFound = error "No page found"
    runCommands = (`withTarget` sampleCommands)

sampleCommands :: TargetClient ()
sampleCommands = do
  traverse_ waitFor [Page.enable, Network.enable Network.defaultEnableParams]

  listener <- onEvent Network.onRequestWillBeSent (liftIO . printRequest)

  void $ waitFor (Page.navigate "http://gitlab.com")
  void $ waitFor Page.onLoadEventFired

  stopEventListener listener

  traverse_ waitFor [Page.disable, Network.disable]

  where
    printRequest = either print print
