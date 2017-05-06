{-# LANGUAGE OverloadedStrings #-}

module EmonicTutor.Data.Card where

import           Data.Aeson
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T

data Card =
  Card { name :: T.Text
       , sets :: [T.Text]
       }
  deriving (Show, Eq)

instance FromJSON Card where
  parseJSON (Object o) =
    Card <$> o .: "name"
         <*> o .: "printings"

  parseJSON _ = fail "Failed to parse card"
