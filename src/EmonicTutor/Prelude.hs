module EmonicTutor.Prelude
  (safeHead
  ) where

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x
