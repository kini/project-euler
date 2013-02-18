-- I basically came up with a similar imperative algorithm as the one
-- described in the overview PDF for this problem, and wrote it in
-- Haskell by using an accumulator variable. What's a more idiomatic
-- way to do this?

import Data.Numbers.Primes

largestPrimeFactor :: Integer -> Integer
largestPrimeFactor n = largestPrimeFactor' primes n
  where
    k `divides` n = n `mod` k == 0
    largestPrimeFactor' (p:ps) n
      | p*p > n = n -- no proper prime factors, so n is prime
      | otherwise = max p $ largestPrimeFactor' ps $ rid n p
    rid n p
      | p `divides` n = rid (n `div` p) p
      | otherwise = n
