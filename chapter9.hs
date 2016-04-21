module Chapter9 where

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

-- function that parameterizes the character
-- you’re breaking the string argument on
-- and rewrite myWords and myLines using it.
mySplit :: Eq a => [a] -> a -> [[a]]
mySplit [] _ = []
mySplit (x:xs) sep
  | x == sep = mySplit xs sep
  | otherwise = takeWhile (/= sep) (x:xs) : mySplit (dropWhile (== sep) . dropWhile (/= sep) $ xs) sep

-- function that split on spaces
myWords :: [Char] -> [[Char]]
myWords sent = mySplit sent ' '

-- function that split on lines
myLines :: [Char] -> [[Char]]
myLines sent = mySplit sent '\n'
