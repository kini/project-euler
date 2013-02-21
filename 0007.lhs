OK, I guess I should actually write my own sieve rather than using
Data.Numbers.Primes ...

This is the naive Sieve of Eratosthenes, more or less.

> divides :: Integer -> Integer -> Bool
> k `divides` n = n `mod` k == 0

> sieve :: [Integer] -> [Integer]
> sieve (p:ns) = p : sieve (filter (not . (p `divides`)) ns)

> primes = sieve [2..]

We can make it a bit faster by doing so-called "wheel factorization"
(see http://en.wikipedia.org/wiki/Wheel_factorization ). With wheel
factorization, we take the first few primes p_1 through p_k and
decompose the positive integers into blocks of length m = lcm(p_1,
..., p_k). We can now search for primes more quickly than with the
naive Sieve of Eratosthenes by making use of the following
observations:

- If n = a*m + b and a = 0, then we can check whether n (i.e. b) is
  prime by using the naive Sieve.

- If n = a*m + b, a > 0, and gcd(b, m) > 1, then this gcd divides both
  b and a*m, and so divides a*m + b = n. If b = 0, then n = a*m = a *
  p_1 * ... * p_k, and because a != 0, we can deduce that n is
  composite; if on the other hand b != 0, then n > m > b >= gcd(b, m)
  | n and so n cannot be prime because it has a proper prime factor.

- Sieve the remaining numbers not ruled out by the above two
  observations.

This can provide a speedup because generating "the remaining numbers
not ruled out by the above two observations" is easy; we can just
generate these numbers for a = 1, and then cycle the resulting list
(adding m each time) to produce the rest of them, out to infinity.

Of course, this speedup won't change the asymptotic time complexity of
the sieving algorithm, just decrease the constant factor.

> wheel k = [ x | x <- [0 .. product ps - 1]
>               , (and . map not) [p `divides` x | p <- ps]]
>   where
>     ps = take k primes

> reducedSearchSpace k =
>   let
>     m = product $ take k primes
>     firstRing = takeWhile (<= m) primes
>     otherRings = [map (+i) (wheel k) | i <- [m, 2*m ..]]
>   in
>    firstRing ++ concat otherRings

> primes' = let k      = 6
>               (a, b) = splitAt k $ reducedSearchSpace k in
>   a ++ sieve b
>
> main = print $ primes' !! 10001

XXX: At least, one would think this would produce a speedup. In
reality, it slows down the search by several seconds! What's going on
here?
