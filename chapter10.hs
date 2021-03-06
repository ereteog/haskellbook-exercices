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

fibs :: [Integer]
fibs = 1 : scanl (+) 1 fibs

fibsFirstN :: Int -> [Integer]
fibsFirstN n = take n fibs


fibsLessThan :: Integer -> [Integer]
fibsLessThan n = takeWhile (< n) fibs

factorial :: Int -> Integer
factorial n = (scanl (*) 1 [1..]) !! n

stopVowelStop :: [Char] -> [Char] -> [(Char, Char, Char)]
stopVowelStop stops vowels = [(x,y,z) | x <- stops, y <- vowels, z <- stops]

pVowelStop :: [Char] -> [Char] -> [(Char, Char, Char)]
pVowelStop stops vowels = [('p',y,z) | y <- vowels, z <- stops]

nounVerbNoun :: [String] -> [String] -> [(String, String, String)]
nounVerbNoun nouns verbs = [(x,y,z) | x <- nouns, y <- verbs, z <- nouns]

avgWordsLength :: String -> Double
avgWordsLength x = (fromIntegral . sum . map length . words $ x)
                   / (fromIntegral . length . words $ x)

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr ((||) . f) False

myElem :: Eq a => a -> [a] -> Bool
myElem el = foldr ((||) . (== el)) False

myElemAny :: Eq a => a -> [a] -> Bool
myElemAny el = any (== el)

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\el acc -> if f el then el : acc else acc) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs) = foldl (\a b ->
                                if LT == (f a b)
                                then a
                                else b) x xs

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = foldl (\a b ->
                                if GT == (f a b)
                                then a
                                else b) x xs
