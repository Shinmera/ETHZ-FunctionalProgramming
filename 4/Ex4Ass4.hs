module Ex4Ass4 where

{-
   In this exercise you are required to adapt the following function implementations of
   f, g and h such that foldl, foldr, zip, zipWith, filter, curry, uncurry, etc. will
   be used. That means, your task is to modify the lines 10-11, 15-19 and 23-28
-}

f :: [[a]] -> [a]
f = concatMap reverse

g :: Eq a => [a] -> [a] -> [a]
g x = map fst . filter (uncurry (==)) . zip x

h :: [Int] -> Int
h = length . filter even
