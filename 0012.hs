-- Use prime factorizations and the fact that triangle numbers are
-- numbers of the form n(n+1)/2

import Data.Numbers.Primes

divides :: Integer -> Integer -> Bool
k `divides` n = n `mod` k == 0

-- I wanted to write this with unfoldr but I was needing ugly looking
-- tuples so I just wrote this (probably equally ugly) accumulating
-- recursive function by hand.
factorize :: Integer -> [Integer]
factorize n = reverse $ factorize' n [] primes
  where
    factorize' 1 as _          = as
    factorize' m [] ps         = factorize' m [0] ps
    factorize' m (a:as) (p:ps)
      | p `divides` m = factorize' (m `div` p) ((a+1):as) (p:ps)
      | otherwise     = factorize' m (0:a:as) ps

numFactors n = product $ map (+1) $ factorize n

val = head $ dropWhile ((<= 500) . numFactors)
      [n*(n+1) `div` 2 | n <- [1..]]

main = print val
