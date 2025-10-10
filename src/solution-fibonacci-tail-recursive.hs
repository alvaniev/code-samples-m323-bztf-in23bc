import GHC.Natural (Natural)




-- Implementatioin using tail recursion
-- Tail recursion does not build up a large stack frame, because the recursive call is the last action in the function
-- Thus it is more memory efficient and faster for large numbers

-- Fib - get fibonacci number at sequence postition n
-- Implementation using tail recursion
-- Two accumulators are used to keep track of the previous two fibonacci numbers
fib :: Natural -> Natural
fib 0  = 0
fib n = go n 0 1
  where
    -- Helper function for tail recursion
    -- 1. number of recursions left (n)
    -- 2. argument (first accumulator): last but one fibonacci number (prevFib)
    -- 3. argument (second accumulator): last fibonacci number (lastFib)
    go :: Natural -> Natural -> Natural -> Natural
    go 1 _ lastFib = lastFib
    go n prevFib lastFib = go (n - 1) lastFib (prevFib + lastFib)


-- Implementation using (:) operator instead of (++) operator
-- (++) operator is inefficient because it has to traverse the whole list to append an element
-- (:) operator is more efficient because it prepends the element to the front of the list
-- Reverse is needed at the end to get the correct order, because (:) prepends to the list
fibseq :: Natural -> [Natural]
fibseq 0 = [0]
fibseq n = reverse (go n [1, 0])
  where
    go :: Natural -> [Natural] -> [Natural]
    go 1 acc = acc
    go k (lastFib : prevFib : xs) = go (k - 1) (lastFib + prevFib : lastFib : prevFib : xs)



fiblist :: Natural -> [(Natural, Natural)]
fiblist n = zip [0 .. n] (fibseq n)
