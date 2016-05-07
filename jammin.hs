module Jammin where

import Data.List

data Fruit = Peach
           | Plum
           | Apple
           | Blackberry
           deriving (Eq, Ord, Show)

data JamJars = Jam {kind :: Fruit,
                    jars :: Int}
             deriving (Eq, Show)

instance Ord JamJars where
  compare (Jam _ n1) (Jam _ n2) = compare n1 n2

countJars :: [JamJars] -> Int
countJars = foldr ((+) . jars) 0

mostJars :: [JamJars] -> JamJars
mostJars [] = undefined
mostJars (x:[]) = x
mostJars (x:xs) = foldr max x xs

compareKind :: JamJars -> JamJars -> Ordering
compareKind (Jam k1 _) (Jam k2 _) = compare k1 k2

sortByKind :: [JamJars] -> [JamJars]
sortByKind = sortBy compareKind

groupJam :: [JamJars] -> [[JamJars]]
groupJam = (groupBy (\j1 j2 -> (compareKind j1 j2) == EQ)) . sortByKind

row1 = Jam Peach 11
row2 = Jam Blackberry 3
row3 = Jam Plum 20
row4 = Jam Peach 10
row5 = Jam Blackberry 4
row6 = Jam Apple 20
allJam = [row1, row2, row3, row4, row5, row6]
