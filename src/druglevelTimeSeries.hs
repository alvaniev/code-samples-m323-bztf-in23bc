import GHC.Natural (Natural)

-- create a time series of drug levels
-- m: daily administered dose
-- r: brake down rate in percentage
-- t: days (length of time series)
druglevelTimeSeries :: Fractional a => a -> a -> Natural -> [(Natural, a)]
-- pattern for day 0, ensure to terminate recursion
druglevelTimeSeries _ _ 0 = [(0, 0)]
-- pattern for subsequent days, prepend current day to previous days
druglevelTimeSeries m r t = (t, currentLevel) : previousDays
  where
    previousDays = druglevelTimeSeries m r (t - 1)
    -- use pattern matching to get second element of first tuple in list
    ((_, previousLevel) : _) = previousDays
    currentLevel = (previousLevel + m) * (1 - r / 100)

