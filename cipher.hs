module Cipher where

import Data.Char

-- caesarShift 0 'a' ==> 'a'
-- caesarShift 26 'a' ==> 'a'
-- caesarShift (-26) 'a' ==> 'a'
-- caesarShift 2 'a' ==> 'c'
-- caesarShift (-2) 'a' ==> 'y'
-- caesarShift (-28) 'a' ==> 'y'
-- caesarShift (28) 'a' ==> 'c'
caesarShift :: Int -> Char -> Char
caesarShift nb letter
  | elem letter ['a'..'z'] = chr $ ord 'a' + (mod (ord letter + nb - ord 'a') 26)
  | elem letter ['A'..'Z'] = chr $ ord 'A' + (mod (ord letter + nb - ord 'A') 26)
  | otherwise = letter

caesar ::  Int -> String -> String
caesar nb = map (caesarShift nb)

uncaesar ::  Int -> String -> String
uncaesar nb = caesar (-nb)
