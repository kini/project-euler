-- Classic naive Haskell solution using the power of data-level
-- recursion to automatically memoize.

val = sum $ takeWhile (<= 4000000) $ filter even fibs
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
