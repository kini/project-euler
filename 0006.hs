-- Naive solution, using NumInstances for fun

import Data.NumInstances

myDiff = (square . sum) - (sum . map square)
  where square n = n * n

val = myDiff [1..100]

main = print val
