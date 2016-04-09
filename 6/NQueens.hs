module NQueens where

{-
   Implement a function that generates all possible board assignments.
-}

generate :: Int -> [[Int]]
generate n = sequence (replicate n [1..n])

{-
   Implement a function that tests whether a given assignment is valid.
-}

test :: [Int] -> Bool
test q = horizontals q
  && diagonals (\x -> x+1) q
  && diagonals (\x -> x-1) q
   where horizontals [] = True
         horizontals (x:xs) = x `notElem` xs && horizontals xs
         diagonals _ [] = True
         diagonals f (x:xs) = innerDiagonals f x xs && diagonals f xs
         innerDiagonals _ _ [] = True
         innerDiagonals f e (x:xs) = f e /= x && innerDiagonals f (f e) xs


naivequeens :: Int -> [[Int]]
naivequeens n = filter test $ generate n


{-
   Headache of the week:
   Implement a function that solves this n queens problem in a more efficient way
   such that partial assignments get tested, whether they may be a valid full assignment,
   as early as possible 
-}

queens :: Int -> [[Int]]
queens = undefined
