primes :: [Integer]
primes = sieve [2..]
  where
    sieve (p:xs) = p : (sieve $ filter ((/=0).(`mod` p)) xs)