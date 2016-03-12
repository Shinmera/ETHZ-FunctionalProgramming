module Words where

split :: Char -> String -> [String]
split _ "" = [""]
split c s = car : if null cdr then [] else split c (drop 1 cdr)
  where (car, cdr) = span (/= c) s

isASpace :: Char -> Bool
isASpace = (== ' ') 

delete :: (Eq a) => a -> [a] -> [a]
delete e a = [x | x <- a, x /= e]
 
toWords :: String -> [String]
toWords = delete "" . split ' '

countWords :: String -> Int
countWords = length . toWords
