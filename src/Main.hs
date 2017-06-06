{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.Reader
import           Control.Applicative ((<|>))
import           EmonicTutor.Config (loadConfigOrDie)
import           EmonicTutor.Types (Tutor)
import           EmonicTutor.SearchCard
import           Snap.Core
import           Snap.Http.Server
import           System.Environment (getEnv)

main :: IO ()
main = do
  config <- loadConfigOrDie
  herokuPort <- read <$> getEnv "PORT"
  httpServe (setPort herokuPort defaultConfig) (runReaderT site config)

site :: Tutor ()
site =
  ifTop (writeBS "This page intentionally left blank.") <|>
  route [ ("mtg", searchCard) ]
