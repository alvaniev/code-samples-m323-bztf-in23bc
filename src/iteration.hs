arithmeticMean :: (Fractional a) => a -> a -> a
arithmeticMean x y = (x + y) / 2

geometricMean :: (Floating a) => a -> a -> a
geometricMean x y = sqrt (x * y)

harmonicMean :: (Fractional a) => a -> a -> a
harmonicMean x y = 2 * (x * y) / (x + y)


-- 1. argument: interpolation method (a function that takes two numbers and returns a number)
-- 2. argument: a list of numbers to interpolate
-- returns: a new list of numbers with interpolated values inserted between each pair of original values
interpolate :: (Fractional a) => (a -> a -> a) -> [a] -> [a]
interpolate _ [] = []
interpolate _ [a] = [a]
-- recursive case: take the first two elements a and b,
-- compute the intermediate value using the interpolation method,
-- and recursively interpolate the rest of the list
interpolate interpolationMethod (a : b : xs) = a : intermediate : interpolate interpolationMethod (b : xs)
  where
    -- compute the intermediate value using the interpolation method
    intermediate = interpolationMethod a b

-- examples of using the interpolate function with different interpolation methods
-- compute the 3rd iteration of interpolation starting from the list [12.0, 24.0, 30.0]
-- each iteration inserts intermediate values between existing values
-- iterate produces an infinite list of successive applications of the interpolation function
-- we take the 3rd element of this infinite list (index 3) to get
a3 = iterate (interpolate arithmeticMean) [12.0, 24.0, 30.0] !! 3

g3 = iterate (interpolate geometricMean) [12.0, 24.0, 30.0] !! 3

h3 = iterate (interpolate harmonicMean) [12.0, 24.0, 30.0] !! 3

-- using a lambda function to define the interpolation method directly,
-- computing the arithmetic mean in this case
a3Lambda = iterate (interpolate (\a b -> (a + b) / 2)) [12.0, 24.0, 30.0] !! 3

-- An infinite list of values starting from 1000.00 and increasing by 2% each step
-- (*1.02) is a function that takes a number and returns that number increased by 2%
-- 1000.00 is the starting value
capitalDevelopment = iterate (* 1.02) 1000.00
