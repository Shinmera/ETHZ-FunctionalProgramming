module PrimeNumbers where

prime :: Int -> Bool
prime n = [x | x <- [1..n], n `mod` x == 0] == [1, n]

allPrimes :: [Int]
allPrimes = [x | x <- [1..], prime x]

primes :: Int -> [Int]
primes m = takeWhile (<= m) allPrimes

firstPrimes :: Int -> [Int]
firstPrimes m = take m allPrimes
