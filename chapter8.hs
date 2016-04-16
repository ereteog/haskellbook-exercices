module Chapter8 where

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
          | (a < 0) && (b > 0) = go a b (q - 1) (r + b)
          | (a > 0) && (b < 0) = go a b (q - 1) (r + b)
          | otherwise = go a b (q + 1) (r - b)
