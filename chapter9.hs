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

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = if x then True else myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = f x || myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem el (x:xs) = (el == x) || myElem el xs

myElem' :: Eq a => a -> [a] -> Bool
myElem' el = any (== el)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish [[]] = []
squish [[], xs] = squish [xs]
squish ((x:xs):xs') = x : squish (xs:xs')

-- Prelude> squishMap (\x -> [1, x, 3]) [2]
-- [1,2,3]
-- Prelude> squishMap (\x -> "WO "++[x]++" HOO ") "123"
-- "WO 1 HOO WO 2 HOO WO 3 HOO "
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = (f x) ++ (squishMap f xs)

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- Prelude> myMaximumBy (\_ _ -> GT) [1..10]
-- 1
-- Prelude> myMaximumBy (\_ _ -> LT) [1..10]
-- 10
-- Prelude> myMaximumBy compare [1..10]
-- 10
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = undefined
myMaximumBy _ [x] = x
myMaximumBy f (x:x':xs) = if (f x x' == GT)
                          then myMaximumBy f (x:xs)
                          else myMaximumBy f (x':xs)

-- Prelude> myMinimumBy (\_ _ -> GT) [1..10]
-- 10
-- Prelude> myMinimumBy (\_ _ -> LT) [1..10]
-- 1
-- Prelude> myMinimumBy compare [1..10]
-- 1
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [] = undefined
myMinimumBy _ [x] = x
myMinimumBy f (x:x':xs) = if (f x x' == LT)
                          then myMinimumBy f (x:xs)
                          else myMinimumBy f (x':xs)

myMaximum :: (Ord a) =>  [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) =>  [a] -> a
myMinimum = myMinimumBy compare
