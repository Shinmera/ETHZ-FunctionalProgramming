module OneTimePad where
import Data.Bits

otp :: [Bool] -> [Bool] -> [Bool]
otp = zipWith xor
