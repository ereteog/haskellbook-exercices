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
