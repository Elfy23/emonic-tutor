{-# LANGUAGE OverloadedStrings #-}

module FindCard where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BSC
import           Data.Char (toLower, toUpper)
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import           EmonicTutor.Config (getCard)
import           EmonicTutor.Data.Card (Card(..), CardName(..), Set(..))
import           EmonicTutor.Data.Slack (SlackMessage, ephemeralSlackMessage, inChannelSlackMessage)
import           EmonicTutor.Types (Tutor)
import           Snap.Core
import           System.Random (randomRIO)

findCard :: Tutor ()
findCard = do
  liftSnap . modifyResponse $ setHeader "Content-Type" "application/json"
  name <- (BSC.map toLower) . (fromMaybe "No Card Name Given") <$> getParam "text"
  maybeCard <- getCard name
  response <- case maybeCard of
                Nothing -> pure . ephemeralSlackMessage $ "Couldn't find: " <> name
                Just card -> cardImageResponse card Nothing
  liftSnap . writeLBS . encode $ response

cardImageResponse :: (MonadIO m) => Card -> Maybe Set -> m SlackMessage
cardImageResponse (Card {cardName=(CardName name), printedSets=sets}) _ = do
  (Set set) <- liftIO $ (sets !!) <$> randomRIO (0, length sets)
  pure . inChannelSlackMessage $ "https://magidex.com/extstatic/card/" <>
                                 (urlEncode . handleSpecialSetRules $ set) <>
                                 "/" <>
                                 urlEncode name <>
                                 ".jpg"

handleSpecialSetRules :: BSC.ByteString -> BSC.ByteString
handleSpecialSetRules setName =
  let firstChar = BSC.head setName
      rest = BSC.tail setName
  in if firstChar == 'p' && BSC.length rest == 3
     then firstChar `BSC.cons`  BSC.map toUpper rest
     else setName
