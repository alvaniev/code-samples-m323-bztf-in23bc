import GHC.Natural (Natural)
import Data.Map (Map, fromList, (!))

-- Implementation using Pascal's triangle and showing the recursive structure
-- However, implementation is inefficient for large n and k due to exponential time complexity
-- Each call to c results in two more calls to c
-- Thus the number of calls grows exponentially with n and k
-- Tail recursive implementation is not possible due to the fact that the result of the two recursive calls
-- need to be added together
c :: Natural -> Natural -> Natural
c n k
  | n < k = error "k must be equal / less than n"
  | n == k || k == 0 = 1
  | otherwise = c (n - 1) (k - 1) + c (n - 1) k





-- Implementation using memoization
-- Memoization is a technique where a data structure is used that represents all
-- the possible outcomes of a function, in this case all the binomial coefficients
-- of the Pascal's triangle up to n and k, implemented using a Map
-- The catch is that due to the lazy evaluation of Haskell, the entire table is not computed at once
-- but rather only the parts that are needed for the computation of c' n k
-- Memoization comes along with self-referential data structures
-- In this case, the Map pascalTriangle is defined by the helper function go
-- which in turn uses the Map pascalTriangle to compute its values
-- This is possible due to Haskell's lazy evaluation, which allows the definition of
-- self-referential data structures
c' :: Natural -> Natural -> Natural
c' n k
  | n < k = error "k must be equal / less than n"
  | otherwise = pascalTriangle ! (n, k)
  where
    -- Use Map as a memoization table (dictionary)
    pascalTriangle :: Map (Natural, Natural) Natural
    -- The list comprehenssion is not evaluated at once, but rather lazily
    -- At this point, pascalTriangle is just a thunk (a structure with deferred computation of its values)
    pascalTriangle = fromList [((i, j), go i j) | i <- [0 .. n], j <- [0 .. k]]
      where
        go :: Natural -> Natural -> Natural
        go i j
          | j == 0 || i == j = 1
          -- At this point, values of pascalTriangle are needed, so they are computed,
          -- if not already evaluated
          | otherwise = pascalTriangle ! (i - 1, j - 1) + pascalTriangle ! (i - 1, j)
