{-# LANGUAGE OverloadedStrings #-}

module FindCard where

import Data.Map.Lazy
import EmonicTutor.Types
import Snap.Core

findCard :: Tutor ()
findCard =
  do
    rq <- liftSnap getRequest
    let params = rqParams rq
    liftSnap $ writeBS . head $ params ! "something"