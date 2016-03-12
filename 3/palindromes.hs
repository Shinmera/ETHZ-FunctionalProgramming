module Palindromes where

palindromes :: [String] -> [String]
palindromes strings = [ x++y | x <- strings, y <- strings, x++y == reverse (x++y) ]
