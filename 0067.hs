-- Recursive solution (same as in problem 18)

levels :: String -> [[Integer]]
levels input = reverse . map (map read . words) . lines $ input

maxes :: [[Integer]] -> [Integer]
maxes [] = []
maxes (l:ls) = let paddedPrev = 0:(maxes ls) ++ [0] in
  zipWith (+) l $ zipWith max paddedPrev (tail paddedPrev)

main = do
  input <- readFile "0067.triangle.txt"
  print $ maximum . maxes . levels $ input
