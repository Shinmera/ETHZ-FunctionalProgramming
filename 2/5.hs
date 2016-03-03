module Coins where

cntChange :: Int -> Int
cntChange x = cntChange' x [500,200,100,50,20,10,5] where
  cntChange' amount coins
    | null coins  = 0
    | amount < 0  = 0
    | amount == 0 = 1
    | otherwise   = cntChange' amount (tail coins) +
                    cntChange' (amount - head coins) coins
