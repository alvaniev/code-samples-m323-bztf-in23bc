import Sampldata
import Vector

-- both versions are logical equivalents
-- the equivalent notation can be explained by the concept of partial application and currying in Haskell


-- version using lambda function
vshift' :: [Vector] -> Vector -> [Vector]
vshift' vecs shiftVec = map (\vec -> vadd shiftVec vec) vecs


-- version using partial application of vadd
-- the expression (vadd shiftVec) is a partially applied function that returns a function which adds shiftVec to its argument
vshift :: [Vector] -> Vector -> [Vector]
vshift vecs shiftVec = map (vadd shiftVec) vecs
