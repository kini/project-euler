-- As is mentioned in the overview PDF, this can be done without any
-- actual programming.

import Data.Numbers.Primes

val = 2^4 * 3^2 * 5 * 7 * 11 * 13 * 17 * 19

-- golfing solution, can handle n = 200,000 in about 12 seconds
lcmOfNumsUntil :: Integer -> Integer
lcmOfNumsUntil n = foldr1 lcm [1..n]

-- efficient solution, can handle n = 1,000,000 in about 9 seconds
lcmOfNumsUntil' :: Integer -> Integer
lcmOfNumsUntil' n = product . map (maxPowerBelow n) . takeWhile (< n) $ primes
  where
    maxPowerBelow n p = last . takeWhile (<n) $ iterate (*p) p

val' = lcmOfNumsUntil' 20

-- solution using logarithms as described in the solution PDF on the
-- Project Euler website; seems to be about the same speed as
-- lcmOfNumsUntil' , though.
lcmOfNumsUntil'' :: Integer -> Integer
lcmOfNumsUntil'' n = product . map (maxPowerBelow n) . takeWhile (< n) $ primes
  where
    maxPowerBelow n p = let [n', p'] = map fromInteger [n, p] in
       p ^ floor (log n' / log p')
