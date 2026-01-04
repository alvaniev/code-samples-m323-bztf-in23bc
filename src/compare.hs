import Data.List
import Sampledata
import Vector

-- Ordering is a built-in Haskell type that can take one of three values: LT, EQ, GT
-- LT means "less than", EQ means "equal to", GT means "greater than"

-- compare two vectors by their y-coordinate
-- imagine a horizontal line sweeping from bottom to top (south to north)
vYCompare :: Vector -> Vector -> Ordering
vYCompare (Vector _ a2) (Vector _ b2)
  | a2 == b2 = EQ
  | a2 < b2 = LT
  | otherwise = GT

-- imagine a 45° line sweeping from north-east to south-west
-- we can achieve this by rotating each vector by 135° (clockwise)and then comparing their y-coordinates,
-- as we did in vYCompare, which is nothing but a horizontal sweep from south to north
vNorthEastCompare :: Vector -> Vector -> Ordering
vNorthEastCompare vecA vecB
  | a2 == b2 = EQ
  | a2 < b2 = LT
  | otherwise = GT
  where
    (Vector _ a2) = vrotate (-135) vecA
    (Vector _ b2) = vrotate (-135) vecB

-- an equivalent implementation of vNorthEastCompare without using vrotate
-- only possible because we know the rotation angle is fixed at 135°
-- thus we can compare the sum of the x- and y-coordinates directly
vNorthEastCompare' :: Vector -> Vector -> Ordering
vNorthEastCompare' (Vector a1 a2) (Vector b1 b2)
  | a1 + a2 == b1 + b2 = EQ
  | a1 + a2 < b1 + b2 = LT
  | otherwise = GT

-- compare two vectors by the angle they make with respect to a given pivot point
-- first argument is the pivot point
-- second and third arguments are the vectors to compare
pivotAngleCompare :: Vector -> Vector -> Vector -> Ordering
pivotAngleCompare pivotPoint vecA vecB
  | thetaA == thetaB = EQ
  | thetaA < thetaB = LT
  | otherwise = GT
  where
    e1 = Vector 1 0
    thetaA = vangle e1 $ vsub vecA pivotPoint
    thetaB = vangle e1 $ vsub vecB pivotPoint

grahamSort :: [Vector] -> [Vector]
grahamSort [] = []
grahamSort [a] = [a]
grahamSort vecs = pivotPoint : sortedOtherPoints
  where
    -- find the point with the lowest y-coordinate (most southern point),
    -- which is the first point after sorting by y-coordinate
    (pivotPoint : otherPoints) = sortBy vYCompare vecs
    -- sort the other points by the angle they make with respect to the pivot point
    -- using the pivotAngleCompare function defined above
    -- (pivotAngleCompare pivotPoint) is a partially applied function that takes two vectors and compares them
    sortedOtherPoints = sortBy (pivotAngleCompare pivotPoint) otherPoints

myCompare :: (String, Integer) -> (String, Integer) -> Ordering
myCompare (_, a) (_, b)
  | a == b = EQ
  | a < b = LT
  | otherwise = GT




