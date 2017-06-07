{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module EmonicTutor.Config
  ( Cards
  , Config(..)
  , getCard
  , getCards
  , loadConfigOrDie
  ) where

import           Control.Monad.Reader
import           Data.Aeson (eitherDecode)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as M
import qualified Data.Text as T
import           EmonicTutor.Data.Card (Card, CardName(..))
import           System.Exit (die)

data Config = Config
  { cards :: Cards
  }

type Cards = M.Map BSC.ByteString Card

getCards :: ( MonadReader Config m ) => m Cards
getCards = asks cards

getCard :: ( MonadReader Config m ) => CardName -> m (Maybe Card)
getCard (CardName name) = M.lookup name <$> getCards

loadConfigOrDie :: IO Config
loadConfigOrDie =
  Config <$> loadCards

loadCards :: IO Cards
loadCards = do
  json <- BSL.readFile "allcards.json"
  let parsed = eitherDecode json
  either die (pure . (M.mapKeys $ BSC.pack . T.unpack . T.toLower)) parsed
