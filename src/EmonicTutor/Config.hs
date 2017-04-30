{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module EmonicTutor.Config where

import           Control.Monad.Reader
import           Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as M
import qualified Data.Text as T
import           EmonicTutor.Data.Card (Card)
import           System.Exit (die)

data Config = Config
  { cards :: M.Map T.Text Card
  }

type Cards = M.Map T.Text Card

getCards :: ( MonadReader Config m ) => m Cards
getCards = asks cards

getCard :: ( MonadReader Config m ) => T.Text -> m (Maybe Card)
getCard name = M.lookup name <$> getCards

loadConfig :: IO Config
loadConfig =
  Config <$> loadCards

loadCards :: IO Cards
loadCards = do
  json <- BSL.readFile "allcards.json"
  let parsed = eitherDecode json
  either die pure parsed