module Chapter10 where

import Data.Time
data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                    deriving (Eq, Ord, Show)
theDatabase :: [DatabaseItem]
theDatabase = [ DbDate (UTCTime
                         (fromGregorian 1911 5 1)
                         (secondsToDiffTime 34123)),
                DbNumber 9001,
                DbString "Hello, world!",
                DbNumber 9,
                DbDate (UTCTime
                         (fromGregorian 1921 5 1)
                         (secondsToDiffTime 34123))]

appendOnlyUTCTime :: DatabaseItem -> [UTCTime] -> [UTCTime]
appendOnlyUTCTime (DbDate utcTime) acc = utcTime : acc
appendOnlyUTCTime _ acc = acc
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr appendOnlyUTCTime []

appendOnlyDbNumber :: DatabaseItem -> [Integer] -> [Integer]
appendOnlyDbNumber (DbNumber n) acc = n : acc
appendOnlyDbNumber _ acc = acc
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr appendOnlyDbNumber []

appendMostRecent :: DatabaseItem -> Maybe UTCTime -> Maybe UTCTime
appendMostRecent (DbDate utcTime) Nothing = Just utcTime
appendMostRecent (DbDate utcTime) (Just currentMin) = Just . max utcTime $ currentMin
appendMostRecent _ acc = acc

mostRecent :: [DatabaseItem] -> Maybe UTCTime
mostRecent = foldr appendMostRecent Nothing

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb db = fromIntegral (sum filtered) / fromIntegral (length filtered)
  where filtered = filterDbNumber db
