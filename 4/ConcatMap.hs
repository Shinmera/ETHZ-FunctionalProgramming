module ConcatMap where

concatMap' :: (a -> [b]) -> [a] -> [b]
concatMap' f = foldr aux e
  where
    aux = (++) . f
    e = []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f v l = undefined
