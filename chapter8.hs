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