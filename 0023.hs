-- Naive solution using sets - pretty slow

import Data.List
import Data.Numbers.Primes
import qualified Data.Set as S

-- reused from problem 12
divides :: Integer -> Integer -> Bool
k `divides` n = n `mod` k == 0

-- reused from problem 12
factorize :: Integer -> [Integer]
factorize n = reverse $ factorize' n [] primes
  where
    factorize' 1 as _          = as
    factorize' m [] ps         = factorize' m [0] ps
    factorize' m (a:as) (p:ps)
      | p `divides` m = factorize' (m `div` p) ((a+1):as) (p:ps)
      | otherwise     = factorize' m (0:a:as) ps

-- reused from problem 21
divisors :: Integer -> [Integer]
divisors n = map (product . zipWith (^) primes)
             (lessers . factorize $ n)
  where
    lessers :: [Integer] -> [[Integer]]
    lessers [] = [[]]
    lessers (a:as) = [a':as' | a' <- [0..a], as' <- lessers as]

counterexamples =
  S.fromList [1..28123] `S.difference` abundantSums
  where
    abundants = [x | x <- [1..28123]
                   , (> 2*x) . sum . divisors $ x]
    abundantSums = gatherSums abundants S.empty
    gatherSums [] set     = set
    gatherSums (x:xs) set = gatherSums xs $
                            foldr S.insert set $
                            takeWhile (<=28123) $
                            map (x+) abundants

val = S.foldr (+) 0 counterexamples

main = print val
