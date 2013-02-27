-- Naive solution

import Data.Numbers.Primes

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

divisors :: Integer -> [Integer]
divisors n = map (product . zipWith (^) primes)
             (lessers . factorize $ n)
  where
    lessers :: [Integer] -> [[Integer]]
    lessers [] = [[]]
    lessers (a:as) = [a':as' | a' <- [0..a], as' <- lessers as]

val = sum amicables
  where
    f x = sum (divisors x) - x
    -- We have to start from 2 to make sure we never leave the
    -- positive integers; f 1 == 0
    nums0 = [2..10000]
    nums1 = map f nums0
    nums2 = map f nums1
    imperfectnesses = zipWith (/=) nums0 nums1
    amicabilities = zipWith (&&) imperfectnesses $
                    zipWith (==) nums0 nums2
    amicables = map snd . filter fst $ zip amicabilities nums0

main = print val
