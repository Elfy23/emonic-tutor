{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.Reader
import           EmonicTutor.Config (loadConfigOrDie)
import           EmonicTutor.Types (Tutor)
import           EmonicTutor.FindCard
import           Snap.Core
import           Snap.Http.Server
import           System.Environment (getEnv)

main :: IO ()
main = do
  config <- loadConfigOrDie
  herokuPort <- read <$> getEnv "PORT"
  httpServe (setPort herokuPort defaultConfig) (runReaderT site config)

site :: Tutor ()
site = route [ ("mtg", findCard) ]
