module Chapter7 where

tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = x `div` 10
        d     = xLast `mod` 10

tensDigit' :: Integral a => a -> a
tensDigit' x = d
  where (xLast, _) = x `divMod` 10
        (_, d)    = xLast `divMod` 10


hundredsDigit :: Integral a => a -> a
hundredsDigit x = d
  where xLast = x `div` 100
        d     = xLast `mod` 10

foldBool :: a -> a -> Bool -> a
foldBool x y True = x
foldBool x y False = y

foldBoolCase :: a -> a -> Bool -> a
foldBoolCase x y expr = case expr of
  True -> x
  False -> y

foldBoolGuard :: a -> a -> Bool -> a
foldBoolGuard  x y expr
  | expr == True = x
  | expr == False = y

g :: (a -> b) -> (a, c) -> (b, c)
g f (x, y) = (f x, y)
