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
import           EmonicTutor.SearchCard.SearchCriteria (getSearchParams, SearchCriteria(..)) 
import           EmonicTutor.Types (Tutor)
import           Snap.Core
import           System.Random (randomRIO)

searchCard :: Tutor ()
searchCard = do
  modifyResponse $ setHeader "Content-Type" "application/json"
  query <- getSearchParams
  response <- case query of
    Left errMsg -> pure . ephemeralSlackMessage $ errMsg
    Right searchCriteria -> 
      case _searchName searchCriteria of
        Nothing -> pure . ephemeralSlackMessage $ "No Card Name Given"
        Just cardName -> do
         maybeCard <- getCard cardName 
         case maybeCard of
          Nothing -> pure . ephemeralSlackMessage $ "No Card Found"
          Just card -> cardImageResponse (_searchSet searchCriteria) card
  writeLBS . encode $ response

cardImageResponse :: (MonadIO m) => Maybe Set -> Card -> m SlackMessage
cardImageResponse Nothing card = do
  let sets = printedSets card
  randomSetIndex <- liftIO $ randomRIO (0, length sets)
  let set = sets !! randomSetIndex
  pure . inChannelSlackMessage $ "https://magidex.com/extstatic/card/" <>
                                 (urlEncode . handleSpecialSetRules . setByteString $ set) <>
                                 "/" <>
                                 (urlEncode . cardNameByteString . cardName $ card) <>
                                 ".jpg"
cardImageResponse (Just set) card =
  if elem set $ printedSets card
  then pure . inChannelSlackMessage $ "https://magidex.com/extstatic/card/" <>
                                 (urlEncode . handleSpecialSetRules . setByteString $ set) <>
                                 "/" <>
                                 (urlEncode . cardNameByteString . cardName $ card) <>
                                 ".jpg"
  else pure . ephemeralSlackMessage $ "Invalid set given."

handleSpecialSetRules :: BSC.ByteString -> BSC.ByteString
handleSpecialSetRules setName =
  let firstChar = BSC.head setName
      rest = BSC.tail setName
  in if firstChar == 'p' && BSC.length rest == 3
     then firstChar `BSC.cons`  BSC.map toUpper rest
     else setName
