module EmonicTutor.Prelude

safeHead :: [a] -> Maybe a
safeHaed [] = Nothing
safeHead x : _ = Just x