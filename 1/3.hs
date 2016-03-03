fibLouis :: Int -> Int
fibLouis 0 = 0
fibLouis 1 = 1
fibLouis n = fibLouis (n-1) + fibLouis (n-2)

fibEva :: Int -> Int
fibEva n = fst (aux n)
  where aux 0 = (0,1)
        aux n = next (aux (n-1))
        next (a,b) = (b, a+b)


{-


fibEva:
f 4 =
fst (aux 4) =
fst (next (aux (4-1)))
fst (next (aux 3))
fst (next (next (aux (3-1))))
fst (next (next (aux 2)))
fst (next (next (next (aux (2-1)))))
fst (next (next (next (aux 1))))
fst (next (next (next (next (aux (1-1))))))
fst (next (next (next (next (aux 0)))))
fst (next (next (next (next (0,1)))))
fst (next (next (next (1,1))))
fst (next (next (1,2)))
fst (next (2,3))
fst (3,5)
3

-}
