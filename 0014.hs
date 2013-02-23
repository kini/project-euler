-- Naive solution. The obvious better solution that comes to mind is
-- to have a mutable array and store the Collatz chain length for each
-- number in a given chain, to avoid the no doubt huge amount of time
-- wasted recomputing all tails of a given Collatz chain. Unfortunately
-- I'm not sure how to do this in Haskell yet so it'll have to wait until
-- I learn more about monads, I think! Anyway, this solution runs in
-- ~10s.

collatz :: Integer -> Integer
collatz n
  | mod n 2 == 0 = n `div` 2
  | otherwise    = 3*n + 1

chainLengths = map (length . takeWhile (/=1) . iterate collatz)
               [1..1000000]

val = maximum $ zip chainLengths [1..]

main = print val
