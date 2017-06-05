{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module EmonicTutor.Types
  ( Tutor
  ) where

import           Control.Monad.Reader
import           EmonicTutor.Config
import           Snap.Core (Snap)

type Tutor a = ReaderT Config Snap a
