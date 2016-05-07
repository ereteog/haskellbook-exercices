{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
module Chapter11 where

data PugType = PugData
data HuskyType a = HuskyData
data DogueDeBordeaux doge = DogueDeBordeaux doge

data Doggies a = Husky a | Mastiff a
               deriving (Eq, Show)

data Price = Price Integer deriving (Eq, Show)
data Size  = Size Double deriving (Eq, Show)
data Manufacturer = Mini
                  | Mazda
                  | Tata
                    deriving (Eq, Show)
data Airline = PapuAir
             | CatapultsR'Us
             | TakeYourChancesUnited
               deriving (Eq, Show)

data Vehicle = Car Manufacturer Price
             | Plane Airline Size
               deriving (Eq, Show)

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _         = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlance _        = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m
getManu _         = undefined

myCar    = Car Mini (Price 14000)
urCar    = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge     = Plane PapuAir (Size 33.5)

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

instance TooMany (Int, String) where
  tooMany (n, s) = tooMany n

instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (n1, n2) = tooMany n1 || tooMany n2

newtype Goats = Goats Int deriving (Eq, Show, TooMany)
newtype Goats' = Goats' (Int, String) deriving (Eq, Show, TooMany)
newtype Goats'' = Goats'' (Int, Int) deriving (Eq, Show, TooMany)

data Person =  Person { name :: String
                      , age :: Int }
            deriving (Eq, Show)


