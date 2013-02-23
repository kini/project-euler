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

-- since n and n+1 must be relatively prime, so too must n/2 and n+1,
-- or n and (n+1)/2 (depending on which one is even, as one of them
-- clearly has to be). Thus the number of factors of their product,
-- i.e. the n-th triangular number, is equal to the product of their
-- numbers of factors.
numFactorsT_i i = product $ map numFactors $
                  if 2 `divides` i then [i `div` 2, i + 1]
                                   else [i, (i + 1) `div` 2]

val = let n = head $ dropWhile ((<= 500) . numFactorsT_i) [1..]
  in n*(n+1) `div` 2 

main = print val
