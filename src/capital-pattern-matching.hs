import GHC.Natural (Natural)
-- function capital
-- d: amount of regular deposits
-- i: interest rate (percentage)
-- t: duration
capital :: Fractional a => a -> a -> Natural -> a
capital _ _ 0 = 0
capital d i t =  previousCapital + d + yield
  where
    previousCapital = capital d i (t - 1)
    yield = (previousCapital + d) * (i / 100)
