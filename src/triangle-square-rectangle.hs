import Vector

equilateralTriangleCornerThree :: Vector -> Vector -> Vector
equilateralTriangleCornerThree vecA vecB = vecC
  where
    -- first step: compute vector AB
    vecAB = vsub vecB vecA
    -- second step: rotate vector AB by 60 degrees, get vector T
    vecT = vrotate 60 vecAB
    -- third step: compute corner C by adding vector T to point A
    vecC = vadd vecA vecT

-- first argument: vecA - first corner of square
-- second argument: vecB - second corner of square
-- result: vecC - third corner of square
squareCornerThree :: Vector -> Vector -> Vector
squareCornerThree vecA vecB = vecC
  where
    -- first step: compute vector AB
    vecAB = vsub vecB vecA
    -- second step: rotate vector AB by 90 degrees, get vector T
    vecT = vrotate 90 vecAB
    -- third step: compute corner C by adding vector T to point B
    vecC = vadd vecB vecT

-- first argument: vecA - first corner of square
-- second argument: vecB - second corner of square
-- result: vecD - fourth corner of square
squareCornerFour :: Vector -> Vector -> Vector
squareCornerFour vecA vecB = vecD
  where
    -- first step: compute vector AB
    vecAB = vsub vecB vecA
    -- second step: rotate vector AB by 90 degrees, get vector T
    vecT = vrotate 90 vecAB
    -- third step: compute corner D by adding vector T to point A
    vecD = vadd vecA vecT 


-- first argument: vecA - first corner of rectangle
-- second argument: vecB - second corner of rectangle
-- third argument: height - height of rectangle
-- result: vecC - third corner of rectangle
rectangleCornerThree :: Vector -> Vector -> Double -> Vector
rectangleCornerThree vecA vecB height = vecC
  where
    -- first step: compute vector AB
    vecAB = vsub vecB vecA
    -- second step: rotate vector AB by 90 degrees, get an unscaled rotated vector
    vecTUnscaled = vrotate 90 vecAB
    -- third step: scale rotated vector to the rectangle height, get vector T
    vecTUnit = vunit vecTUnscaled
    vecT = vmult height vecTUnit
    -- fourth step: compute corner C by adding vector T to point B
    vecC = vadd vecB vecT

-- first argument: vecA - first corner of rectangle
-- second argument: vecB - second corner of rectangle
-- third argument: height - height of rectangle
-- result: vecC - third corner of rectangle
rectangleCornerFour :: Vector -> Vector -> Double -> Vector
rectangleCornerFour vecA vecB height = vecD
  where
    -- first step: compute vector AB
    vecAB = vsub vecB vecA
    -- second step: rotate vector AB by 90 degrees, get an unscaled rotated vector
    vecTUnscaled = vrotate 90 vecAB
    -- third step: scale rotated vector to the rectangle height, get vector T
    vecTUnit = vunit vecTUnscaled
    vecT = vmult height vecTUnit
    -- fourth step: compute corner D by adding vector T to point A
    vecD = vadd vecA vecT