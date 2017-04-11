{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server
import           FindCard

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site = route [ ("mtg", findCard) ]
