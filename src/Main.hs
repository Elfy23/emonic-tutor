{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.Reader
import           EmonicTutor.Config (loadConfigOrDie)
import           EmonicTutor.Types (Tutor)
import           FindCard
import           Snap.Core
import           Snap.Http.Server

main :: IO ()
main = do
  config <- loadConfigOrDie
  quickHttpServe (runReaderT site config)

site :: Tutor ()
site = route [ ("mtg", findCard) ]
