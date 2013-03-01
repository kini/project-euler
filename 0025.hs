-- Naive solution

import Data.List

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

val = let
  Just (val, _) = find ((>= 1000) . length . show . snd) (zip [0..] fibs)
  in val

main = print val
