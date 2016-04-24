module Chapter9 where

import Data.Char

eftBool :: Bool -> Bool -> [Bool]
eftBool False False = [False]
eftBool False True = [False, True]
eftBool True True = [True]
eftBool True False = []

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd GT GT = [GT]
eftOrd x y
  | x > y = []
  | otherwise = x : eftOrd (succ x) y

eftInt :: Int -> Int -> [Int]
eftInt x y
  | (x == (maxBound :: Int))
    && (y == (maxBound :: Int)) = [(maxBound :: Int)]
  | x > y = []
  | otherwise = x : eftInt (succ x) y

eftChar :: Char -> Char -> [Char]
eftChar x y
  | (x == (maxBound :: Char))
    && (y == (maxBound :: Char)) = [(maxBound :: Char)]
  | x > y = []
  | otherwise = x : eftChar (succ x) y

-- function that split a list on a parametrized
-- value and rewrite myWords and myLines using it.
mySplit :: Eq a => [a] -> a -> [[a]]
mySplit [] _ = []
mySplit (x:xs) sep
  | x == sep = mySplit xs sep
  | otherwise = takeWhile (/= sep) (x:xs) : mySplit (dropWhile (== sep) . dropWhile (/= sep) $ xs) sep

-- function that split on lines
myLines :: [Char] -> [[Char]]
myLines sent = mySplit sent '\n'

-- function that split on spaces
myWords :: [Char] -> [[Char]]
myWords sent = mySplit sent ' '

-- words without "the", "a", "an"
-- myWordsWoStopwords "the brown dog was a goof"
-- => ["brown","dog","was","goof"]
myWordsWoStop :: [Char] -> [[Char]]
myWordsWoStop sent = filter (\x -> not (elem x ["the", "a", "an"])) . myWords $ sent

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys

myZip :: [a] -> [b] -> [(a, b)]
myZip xs ys = myZipWith (,) xs ys

-- function that filters all the uppercase letters out of a String
-- example: keepUpper "HbEfLrLxO," ==> "HELLO."
keepUpper :: [Char] -> [Char]
keepUpper xs = filter isUpper xs

-- function that will capitalize the first letter of a String
-- and return the entire String.
-- example: capitalizeFirst "audrey" => "Audrey"
capitalizeFirst :: [Char] -> [Char]
capitalizeFirst [] = []
capitalizeFirst (x:xs) = toUpper x : xs

-- capitalize every letter of a String
-- example: allCaps "audrey" => "AUDREY"
allCaps :: [Char] -> [Char]
allCaps xs = map toUpper xs
