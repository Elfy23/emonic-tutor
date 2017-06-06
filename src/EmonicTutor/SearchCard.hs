{-# LANGUAGE OverloadedStrings #-}

module EmonicTutor.SearchCard
  ( searchCard
  ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BSC
import           Data.Char (toUpper)
import           Data.Monoid ((<>))
import           EmonicTutor.Config (getCard)
import           EmonicTutor.Data.Card (Card(..), CardName(..), Set(..), cardNameByteString)
import           EmonicTutor.Data.Slack (SlackMessage, ephemeralSlackMessage, inChannelSlackMessage)
import           EmonicTutor.Types (Tutor)
import           Snap.Core
import           System.Random (randomRIO)

searchCard :: Tutor ()
searchCard = do
  modifyResponse $ setHeader "Content-Type" "application/json"
  query <- getParam "text"
  response <- case query of
    Nothing -> pure . ephemeralSlackMessage $ "No Card Name Given"
    Just "" -> pure . ephemeralSlackMessage $ "No Card Name Given"
    Just name -> do
      maybeCard <- getCard name
      maybe (pure . ephemeralSlackMessage $ "Couldn't find: " <> name)
            (cardImageResponse Nothing) maybeCard
  writeLBS . encode $ response

-- The Maybe Set here is for use with user defined sets in the future
cardImageResponse :: (MonadIO m) => Maybe Set -> Card -> m SlackMessage
cardImageResponse _ card = do
  let sets = printedSets card
  randomSetIndex <- liftIO $ randomRIO (0, length sets)
  let set = sets !! randomSetIndex
  pure . inChannelSlackMessage $ "https://magidex.com/extstatic/card/" <>
                                 (urlEncode . handleSpecialSetRules . setByteString $ set) <>
                                 "/" <>
                                 (urlEncode . cardNameByteString . cardName $ card) <>
                                 ".jpg"

handleSpecialSetRules :: BSC.ByteString -> BSC.ByteString
handleSpecialSetRules setName =
  let firstChar = BSC.head setName
      rest = BSC.tail setName
  in if firstChar == 'p' && BSC.length rest == 3
     then firstChar `BSC.cons`  BSC.map toUpper rest
     else setName
