-- compute gcd (Euclidian algorithm - greatest common divisor)
egcd :: Integral a => a -> a -> a
egcd p q
  | remainder == 0 = n
  | otherwise = egcd n remainder
  where
    m = max (abs p) (abs q)
    n = min (abs p) (abs q)
    remainder = mod m n