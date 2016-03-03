module SquareRoot where
import Control.Monad

eps :: Double
eps = 0.001

root :: Double -> Double
root number =
  root' 1 number
  where root' previous next
          | goodEnough previous next = next
          | otherwise = root' next (improve number next)

improve :: Double -> Double -> Double
improve number approximation = (approximation + (number / approximation)) / 2

goodEnough :: Double -> Double -> Bool
goodEnough previous next = abs ((next - previous) / previous) < eps

main :: IO ()
main = do
  putStrLn "Compute the root of:"
  line <- getLine
  unless (null line) $ do
    let number = read line
    when (0 < number) $ do
      putStrLn ("Square root: " ++ show (root number))
      main
