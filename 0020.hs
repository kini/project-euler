-- Naive solution

digitalSum 0 = 0
digitalSum n = mod n 10 + digitalSum (div n 10)

factorial n = product [1..n]

val = digitalSum . factorial $ 100

main = print val
