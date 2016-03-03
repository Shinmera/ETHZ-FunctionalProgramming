data Complex_ = Complex_ Double Double

re :: Complex_ -> Double
re (Complex_ real imag) = real

im :: Complex_ -> Double
im (Complex_ real imag) = imag

conj :: Complex_ -> Complex_
conj (Complex_ real imag) = (Complex_ real (-imag))

add :: Complex_ -> Complex_ -> Complex_
add (Complex_ r1 i1) (Complex_ r2 i2) = (Complex_ (r1+r2) (i1+i2))

mult :: Complex_ -> Complex_ -> Complex_
mult (Complex_ r1 i1) (Complex_ r2 i2) = (Complex_ (r1*r2 - i1*i2) (r1*i2 + i1*r2))

absv :: Complex_ -> Double
absv (Complex_ real imag) = sqrt (real^2 + imag^2)

main :: IO ()
main = do
  putStrLn "Enter your complex number's real component:"
  real <- getLine
  putStrLn "Enter your complex number's imaginary component:"
  imag <- getLine
  let complex = (Complex_ (read real :: Double) (read imag :: Double))
    in do
      putStrLn ("Your complex number's absolute value is: " ++ show(absv complex))

{-

module Complex where
-- write your implementation of complex numbers here

re :: (Double, Double) -> Double
re (real, imag) = real

im :: (Double, Double) -> Double
im (real, imag) = imag

conj :: (Double, Double) -> (Double, Double)
conj (real, imag) = (real, (-imag))

add :: (Double, Double) -> (Double, Double) -> (Double, Double)
add (r1, i1) (r2, i2) = ((r1+r2), (i1+i2))

mult :: (Double, Double) -> (Double, Double) -> (Double, Double)
mult (r1, i1) (r2, i2) = ((r1*r2 - i1*i2), (r1*i2 + i1*r2))

absv :: (Double, Double) ->  Double
absv (real, imag) = sqrt (real^2 + imag^2)

main :: IO ()
main = do
  putStrLn "Enter your complex number's real component:"
  real <- getLine
  putStrLn "Enter your complex number's imaginary component:"
  imag <- getLine
  let complex = ((read real :: Double), (read imag :: Double))
    in do
      putStrLn ("Your complex number's absolute value is: " ++ show(absv complex))

-}
