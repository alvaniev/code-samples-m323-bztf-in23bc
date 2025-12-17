import Sampledata
import Vector

-- both versions are logical equivalents
-- the equivalent notation can be explained by the concept of partial application and currying in Haskell

-- version using partial application of function filter
-- this notation expresses that pointsNW returns a filter-function that takes a list of vectors and returns a list of filtered vectors
pointsNW :: [Vector] -> [Vector]
pointsNW = filter (\(Vector x y) -> (x <= 0) && (y >= 0))

-- version using explicit arguments of function filter
-- this notation expresses that pointsNW' takes a list of vectors and returns a list of filtered vectors
pointsNW' :: [Vector] -> [Vector]
pointsNW' vecs = filter (\(Vector x y) -> (x <= 0) && (y >= 0)) vecs

