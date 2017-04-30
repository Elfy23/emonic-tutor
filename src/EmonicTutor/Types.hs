{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module EmonicTutor.Types where

import           Control.Applicative
import           Control.Monad.Reader
import qualified Data.ByteString.Char8 as BSC
import           Data.Map
import qualified Data.Text as T
import           EmonicTutor.Config
import           Snap.Core (Snap, MonadSnap, liftSnap, rqParams, getRequest)

type Tutor a = ReaderT Config Snap a

getParams :: ( MonadSnap m ) => m (Map BSC.ByteString [T.Text])
getParams =
  (fmap . fmap . fmap) (T.pack . BSC.unpack) (rqParams <$> liftSnap getRequest)
  --   liftSnap getRequest :: Snap Request
  --   rqParams <$> liftSnap getRequest :: Snap Params or Snap (Map BSC.ByteString [BSC,ByteString])
  --   (fmap . fmap . fmap) (T.pack . BSC.unpack) (rqParams <$> liftSnap getRequest) ::Snap (Map BSC.ByteString [T.Text])