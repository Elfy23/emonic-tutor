{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module EmonicTutor.Data.Card
  ( Card(..)
  , CardName(..)
  , Set(..)
  ) where

import           Data.Aeson
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T

data Card =
  Card { cardName :: CardName
       , printedSets :: [Set]
       }
  deriving (Show, Eq)

instance FromJSON Card where
  parseJSON (Object o) =
    Card <$> o .: "name"
         <*> o .: "printings"

  parseJSON _ = fail "Failed to parse card"

newtype CardName = CardName BSC.ByteString
  deriving(Show, Eq)

instance FromJSON CardName where
  parseJSON (String name) = pure . CardName . BSC.pack . T.unpack . T.toLower $ name

  parseJSON _ = fail "Failed to parse CardName"

newtype Set = Set BSC.ByteString
  deriving(Show, Eq)

instance FromJSON Set where
  parseJSON (String set) = pure . Set . BSC.pack . T.unpack . T.toLower $ set

  parseJSON _ = fail "Failed to parse Set"
