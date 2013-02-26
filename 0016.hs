-- Naive solution

val = sum . map (read . (:[])) . show $ 2 ^ 1000

main = print val
