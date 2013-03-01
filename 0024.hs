import Data.List

makeBase10 :: [Integer] -> Integer
makeBase10 = sum . zipWith (\x y -> y * 10^x) [0..] . reverse

-- Naive solution
val = makeBase10 $ (!! 999999) . sort . permutations $ [0..9]

-- Smarter solution
nthPermutation :: Ord a => Integer -> [a] -> [a]
nthPermutation n xs
  = helper n (reverse $ zipWith const facs xs) [] xs
  where
    helper 0 _ [] rs = rs
    helper n (k:ks) ls (r:rs)
      | n - k < 0 = r : helper n ks [] (reverse ls ++ rs)
      | otherwise = helper (n - k) (k:ks) (r:ls) rs
    facs = scanl (*) 1 [1..]

val' = makeBase10 $ nthPermutation 999999 [0..9]

main = print val'
