module Chapter8 where

import Data.List (intersperse)

func :: [a] -> [a] -> [a]
func x y = x ++ y


cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

  -- fill in the types
flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe ::  String -> String
frappe = flippy "haha"

-- Write a function that recursively sums all numbers
-- from 1 to n, n being the argument. So that if n was 5,
-- you’d add 1 + 2 + 3 + 4 + 5 to get 15.
sum1Ton :: (Eq a, Num a) => a -> a
sum1Ton 1 = 1
sum1Ton n = n + (sum1Ton (n - 1))

-- Write a function that multiplies two integral numbers
-- using recursive summation.
multRec :: (Integral a) => a -> a -> a
multRec 0 _ = 0
multRec _ 0 = 0
multRec x 1 = x
multRec x y
  | x < y = multRec y x
  | otherwise = x + (multRec x (y - 1))


-- implement recursive version of divMod with relative numbers
-- divModRec  10 3 == Result 3 1
-- divModRec  10 (-3) == Result (-4) (-2)
-- divModRec  (-10) (-3) == Result 3 (-1)
-- divModRec  (-10) 3 == Result (-4) 2
data DivModResult = Result Integer Integer
                   | DividedByZero deriving Show

divModRec :: Integer -> Integer -> DivModResult
divModRec num denom = go num denom 0 num
  where go a b q r
          | b == 0 = DividedByZero
          | (a == (b*q + r))
            && ((abs r) < (abs b))
            && ((abs (b - r)) < (abs b)) = Result q r
          | (a < 0) && (b > 0)
            || (a > 0) && (b < 0) = go a b (q - 1) (r + b)
          | otherwise = go a b (q + 1) (r - b)

-- The McCarthy 91 function yields 𝑥 − 10 when 𝑥 > 100 and 91 otherwise. The function is recursive.
mc91 :: Integer -> Integer
mc91 n
  | n > 100 = n - 10
  | otherwise = 91

-- digitToWord turns integers from 1-9 into their corresponding English words, ”one,” ”two,” and so on.
digitToWord :: Int -> String
digitToWord n = case n of
  0 -> "zero"
  1 -> "one"
  2 -> "two"
  3 -> "three"
  4 -> "four"
  5 -> "five"
  6 -> "six"
  7 -> "seven"
  8 -> "eight"
  9 -> "nine"
  _ -> "not a valid number"

-- digits takes the integer, separates the digits, and returns it as a list of integers.
digits :: Int -> [Int]
digits n
  | n < 10 = [n]
  | otherwise = digits (div n 10) ++ [mod n 10]

-- wordNumber returns the English word version of the Int value.
wordNumber :: Int -> String
wordNumber n = concat . intersperse "-" . map digitToWord . digits $ n
