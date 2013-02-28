-- Naive solution (not that there's really a smarter solution afaict)

import Data.Char
import Data.List
import Text.CSV

-- pointless programming is fun
scores = zipWith (*) [1..]
         . map (sum . map (subtract (ord' 'A' - 1) . ord'))
         . sort
  where ord' = fromIntegral . ord

main = do
  result <- parseCSVFromFile "0022.names.txt"
  -- pattern-matching is handy
  let Right (text:_) = result
  print . sum . scores $ text
