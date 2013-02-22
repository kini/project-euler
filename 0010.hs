-- This time, we'd better just use a finite sieve.

divides :: Integer -> Integer -> Bool
k `divides` n = n `mod` k == 0

sieveUntil :: Integer -> [Integer] -> [Integer]
sieveUntil limit (p:ns)
  | p*p > limit = takeWhile (< limit) (p:ns)
  | otherwise = p:(sieveUntil limit (filter (not . (p `divides`)) ns))

val = sum $ sieveUntil 2000000 [2..]

main = print val
