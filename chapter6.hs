module Chapter6 where

import Data.List

jung :: [Int] -> Int
jung xs = head (sort xs)

young :: Ord a => [a] -> a
young xs = head (sort xs)

mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
signifier xs = head (mySort xs)

data TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
    (==) (TisAn x) (TisAn x') = x == x'

data TwoInteger = Two Integer Integer

instance Eq TwoInteger where
    (==) (Two x y) (Two x' y') = (x == x') && (y == y')

data StringOrInt =
    TisAnInt Int
    | TisAString String

instance Eq StringOrInt where
    (==) (TisAnInt x) (TisAnInt x') = x == x'
    (==) (TisAString x) (TisAString x') = x == x'

data Pair a =
    Pair a a

instance Eq a => Eq (Pair a) where
    (==) (Pair x y) (Pair x' y') = (x == x') && (y == y')

data Tuple a b =
    Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple x y) (Tuple x' y') = (x == x') && (y == y')

data Which a = ThisOne a | ThatOne a

instance Eq a => Eq (Which a) where
    (==) (ThisOne x) (ThisOne x') = x == x'
    (==) (ThatOne x) (ThatOne x') = x == x'
    (==) (ThatOne x) (ThisOne x') = False
    (==) (ThisOne x) (ThatOne x') = False

data EitherOr a b = Hello a | GoodBye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) (Hello x) (Hello x') = x == x'
    (==) (GoodBye x) (GoodBye x') = x == x'
    (==) (GoodBye x) (Hello x') = False
    (==) (Hello x) (GoodBye x') = False