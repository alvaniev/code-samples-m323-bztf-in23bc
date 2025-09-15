import GHC.Natural (Natural)

-- this implementation has two weaknesses:
-- 1. it uses (++) operator which is inefficient for large lists
-- 2. it uses non-tail recursion which builds up a large stack frame
fibseq :: Natural -> [Natural]
fibseq 0 = [0]
fibseq 1 = [0, 1]
fibseq n = prevSeq ++ [newFib]
  where
    -- Get the Fibonacci sequence up to n-1
    prevSeq = fibseq (n - 1)
    -- Get the last two Fibonacci numbers from the previous sequence
    prevPrevFib = last (init prevSeq)
    prevFib = last prevSeq
    -- Calculate the new Fibonacci number
    newFib = prevFib + prevPrevFib


fib :: Natural -> Natural
fib n = last (fibseq n)

fiblist :: Natural -> [(Natural, Natural)]
fiblist n = zip [0 .. n] (fibseq n)


