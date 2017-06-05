{-# LANGUAGE OverloadedStrings #-}
module EmonicTutor.Data.Slack
  ( SlackMessage(..)
  , ephemeralSlackMessage
  , inChannelSlackMessage
  ) where

import           Data.Aeson
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T

data SlackMessage = SlackMessage
  { responseType :: ResponseType
  , slackText :: SlackText
  }
  deriving (Show, Eq)

instance ToJSON SlackMessage where
  toJSON slackMessage =
    object [ "response_type" .= responseType slackMessage
           , "text" .= slackText slackMessage
           ]

data ResponseType = InChannel
                  | Ephemeral
  deriving (Show, Eq)

instance ToJSON ResponseType where
  toJSON InChannel = "in_channel"
  toJSON Ephemeral = "ephemeral"

newtype SlackText = SlackText BSC.ByteString
  deriving (Show, Eq)

instance ToJSON SlackText where
  toJSON (SlackText text) = String . T.pack . BSC.unpack $ text

inChannelSlackMessage :: BSC.ByteString -> SlackMessage
inChannelSlackMessage = SlackMessage InChannel . SlackText

ephemeralSlackMessage :: BSC.ByteString -> SlackMessage
ephemeralSlackMessage = SlackMessage Ephemeral . SlackText
