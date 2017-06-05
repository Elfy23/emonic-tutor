{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module EmonicTutor.Types
  ( Tutor
  , getParams
  ) where

import           Control.Monad.Reader
import qualified Data.ByteString.Char8 as BSC
import           Data.Map
import qualified Data.Text as T
import           EmonicTutor.Config
import           Snap.Core (Snap, MonadSnap, liftSnap, rqParams, getRequest)

type Tutor a = ReaderT Config Snap a

getParams :: ( MonadSnap m ) => m (Map BSC.ByteString [T.Text])
getParams = convertMap . rqParams <$> liftSnap getRequest

convertMap :: Map BSC.ByteString [BSC.ByteString] -> Map BSC.ByteString [T.Text]
convertMap = fmap . fmap $ T.pack . BSC.unpack
