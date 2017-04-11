{-# LANGUAGE OverloadedStrings #-}

module FindCard where

import Data.Map.Lazy
import Snap.Core

findCard :: Snap ()
findCard =
  do
    rq <- getRequest
    let params = rqParams rq
    writeBS . head $ params ! "something"