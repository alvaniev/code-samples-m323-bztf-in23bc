import Vector
import Sampledata

-- foldl iterates over al list of Vectors
-- accumulator is initialized with the y-value of the first Vector in the list
-- for each Vector in the list, the lambda function compares its y-value with the current minimum y-value stored in the accumulator
-- after processing all Vectors, the accumulator holds the minimum y-value found in the list
minY :: [Vector] -> Double
minY [] = error "Liste muss mindestens ein Vector haben"
minY ((Vector _ y) : vecs) = foldl (\acc (Vector _ y) -> min y acc) y vecs