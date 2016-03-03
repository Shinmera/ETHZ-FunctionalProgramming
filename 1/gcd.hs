---- Some examples

--- renamed as "gcd" already taken in Prelude
gcdiv :: Int -> Int -> Int
gcdiv x y                         
 | x == y      = x
 | y > x       = gcdiv x (y-x)
 | otherwise   = gcdiv (x-y) y

gcdInt :: Int -> Int -> Int
gcdInt x y = gcdiv (abs x) (abs y)

xor x y
 | x == True   = not y
 | x == False  = y

dups n s
 | n == 0      = ""
 | n >  0      = s ++ (dups (n-1) s)
 
silly b (x,y)
    | b            = x + y
    | otherwise    = x * y

fac 0 = 1
fac n = n * fac (n-1)

f1 0 = 0
f1 x = g1 (x-1)

g1 0 = 0
g1 x = 1 + f1 (x - 1)

