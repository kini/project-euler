-- No computation needed; this is of course just $\binom{40}{20}$

factorial n = product [2..n]

val = factorial 40 `div` (factorial 20 * factorial 20)
