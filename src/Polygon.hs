{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Polygon
  ( Polygon (EquilateralTriangle, Square, Rectangle),
    corners,
  )
where

-- import Data.List
-- import Fractal
-- import Text.Read
import Vector

-- keyword data: definition of new type
-- also called: TYPE CONSTRUCTOR
data Polygon where

  -- VALUE CONSTRUCTORS (sometimes called data constructors)
  
  -- name of VALUE CONSTRUCTOR: Square
  -- arguments of VALUE CONSTRUCTOR: two Vectors representing two corner points of the square
  -- result of VALUE CONSTRUCTOR: Polygon
  Square :: Vector -> Vector -> Polygon
  
  -- name of VALUE CONSTRUCTOR: Rectangle
  -- arguments of VALUE CONSTRUCTOR: two Vectors representing two corner points of the rectangle and a Double representing the height
  -- result of VALUE CONSTRUCTOR: Polygon
  Rectangle :: Vector -> Vector -> Double -> Polygon

  -- name of VALUE CONSTRUCTOR: EquilateralTriangle
  -- arguments of VALUE CONSTRUCTOR: two Vectors representing two corner points of the triangle
  -- result of VALUE CONSTRUCTOR: Polygon
  EquilateralTriangle :: Vector -> Vector -> Polygon

  deriving (Show)

-- Return all corner points of polygone as a list of vectors
corners :: Polygon -> [Vector]
-- Pattern matching a Square
corners (Square vecA vecB) = [vecA, vecB, vecC, vecD]
  where
    ortho = vrotate 90 $ vsub vecB vecA
    vecC = vadd vecB ortho
    vecD = vadd vecA ortho

-- Pattern matching a Rectangle
corners (Rectangle vecA vecB h) = [vecA, vecB, vecC, vecD]
  where
    ortho = vmult h $ vunit $ vrotate 90 $ vsub vecB vecA
    vecC = vadd vecB ortho
    vecD = vadd vecA ortho

-- Pattern matching an Equilateral Triangle
corners (EquilateralTriangle vecA vecB) = [vecA, vecB, vecC]
  where
    rotatedSide = vrotate 60 $ vsub vecB vecA
    vecC = vadd vecA rotatedSide


