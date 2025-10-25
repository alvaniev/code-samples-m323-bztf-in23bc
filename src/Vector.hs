{-# LANGUAGE GADTs #-}
module Vector
  ( Vector (Vector),
    vadd,
    vsub,
    vlength,
    vunit,
    vrotate,
    vmult,
    vdotproduct,
    vangle,
    vcrossproduct,
  )
where

-- keyword data: definition of new type
-- somtimes called: TYPE CONSTRUCTOR
-- (not to be confused with the
-- VALUE CONSTRUCTOR within type definition)
data Vector where
  -- VALUE CONSTRUCTOR (sometimes called
  -- DATA CONSTRUCTOR): Vector
  -- arguments of constructor:
  -- two Doubles, namely x- and y-value
  -- result of constructor: Vector
  Vector :: Double -> Double -> Vector
  -- Vector shall be an instance of type class Show
  -- type class Show provides a function
  -- which can print out a Vector to a String
  deriving (Show)

vadd :: Vector -> Vector -> Vector
vadd (Vector a1 a2) (Vector b1 b2) = Vector (a1 + b1) (a2 + b2)

vsub :: Vector -> Vector -> Vector
vsub (Vector a1 a2) (Vector b1 b2) = Vector (a1 - b1) (a2 - b2)

vlength :: Vector -> Double
vlength (Vector a1 a2) = sqrt (a1 ^ 2 + a2 ^ 2)

vunit :: Vector -> Vector
vunit (Vector a1 a2) = Vector (a1 / length) (a2 / length)
  where
    length = vlength (Vector a1 a2)

vmult :: Double -> Vector -> Vector
vmult q (Vector a1 a2) = Vector (a1 * q) (a2 * q)

vrotate :: Double -> Vector -> Vector
vrotate phi (Vector a1 a2) = Vector (a1 * cos rphi - a2 * sin rphi) (a1 * sin rphi + a2 * cos rphi)
  where
    rphi = phi * (pi / 180)

vdotproduct :: Vector -> Vector -> Double
vdotproduct (Vector a1 a2) (Vector b1 b2) = a1 * b1 + a2 * b2

vangle :: Vector -> Vector -> Double
vangle a b = acos (vdotproduct a b / (vlength a * vlength b))

vcrossproduct :: Vector -> Vector -> Double
vcrossproduct (Vector a1 a2) (Vector b1 b2) = a1 * b2 - a2 * b1
