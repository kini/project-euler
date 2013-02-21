-- Naive solution

vals = [a*b*c | a <- [0 .. 1000]
         , b <- [a + 1 .. 1000 - a]
         , c <- [1000 - a - b]
         , c > b
         , a*a + b*b == c*c]

val = head vals
