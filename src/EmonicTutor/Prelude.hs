module EmonicTutor.Prelude

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead x : _ = Just x