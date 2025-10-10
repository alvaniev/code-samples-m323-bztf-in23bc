import GHC.Natural (Natural)
-- function capital
-- d: amount of regular deposits
-- i: interest rate (percentage)
-- t: duration
capital :: Fractional a => a -> a -> Natural -> a
capital d i t
  | t == 0 = 0
  | otherwise = previousCapital + d + yield
  where
    previousCapital = capital d i (t - 1)
    yield = (previousCapital + d) * (i / 100)

    
