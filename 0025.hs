-- Naive solution

import Data.List

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

val = let Just val = find ((>= 1000) . length . show) fibs in val

main = print val
