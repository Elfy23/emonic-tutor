{-# LANGUAGE OverloadedStrings #-}

module EmonicTutor.Data.Card where

import           Data.Aeson
import qualified Data.ByteString.Char8 as BSC

data Card =
  Card { name :: BSC.ByteString
       , sets :: [BSC.ByteString]
       }
  deriving (Show, Eq)

instance FromJSON Card where
  parseJSON (Object o) =
    Card <$> (BSC.pack <$> o .: "name")
         <*> (fmap.fmap) BSC.pack (o .: "printings")

  parseJSON _ = fail "Failed to parse card"
