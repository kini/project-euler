-- A naive Haskell solution (0.62 seconds)

isPalindrome :: (Num a, Show a) => a -> Bool
isPalindrome a = show a == reverse (show a)

val = maximum $ filter isPalindrome $ [x*y | x <- [100..999], y <- [x..999]]

main = print val
