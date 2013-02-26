-- Naive solution

import Data.List

sayNumber :: Integer -> String
sayNumber 0 = "zero"
sayNumber 1 = "one"
sayNumber 2 = "two"
sayNumber 3 = "three"
sayNumber 4 = "four"
sayNumber 5 = "five"
sayNumber 6 = "six"
sayNumber 7 = "seven"
sayNumber 8 = "eight"
sayNumber 9 = "nine"
sayNumber 10 = "ten"
sayNumber 11 = "eleven"
sayNumber 12 = "twelve"
sayNumber 13 = "thirteen"
sayNumber 14 = "fourteen"
sayNumber 15 = "fifteen"
sayNumber 16 = "sixteen"
sayNumber 17 = "seventeen"
sayNumber 18 = "eighteen"
sayNumber 19 = "nineteen"
sayNumber 20 = "twenty"
sayNumber 30 = "thirty"
sayNumber 40 = "forty"
sayNumber 50 = "fifty"
sayNumber 60 = "sixty"
sayNumber 70 = "seventy"
sayNumber 80 = "eighty"
sayNumber 90 = "ninety"
sayNumber x
  | x < 0 = "negative " ++ sayNumber (-x)
  | x >= 1000^21 = separate x (1000^21) "vigintillion"
  | x >= 1000^20 = separate x (1000^20) "novemdecillion"
  | x >= 1000^19 = separate x (1000^19) "octodecillion"
  | x >= 1000^18 = separate x (1000^18) "septendecillion"
  | x >= 1000^17 = separate x (1000^17) "sexdecillion"
  | x >= 1000^16 = separate x (1000^16) "quindecillion"
  | x >= 1000^15 = separate x (1000^15) "quattuordecillion"
  | x >= 1000^14 = separate x (1000^14) "tredecillion"
  | x >= 1000^13 = separate x (1000^13) "duodecillion"
  | x >= 1000^12 = separate x (1000^12) "undecillion"
  | x >= 1000^11 = separate x (1000^11) "decillion"
  | x >= 1000^10 = separate x (1000^10) "nonillion"
  | x >= 1000^9  = separate x (1000^9 ) "octillion"
  | x >= 1000^8  = separate x (1000^8 ) "septillion"
  | x >= 1000^7  = separate x (1000^7 ) "sextillion"
  | x >= 1000^6  = separate x (1000^6 ) "quintillion"
  | x >= 1000^5  = separate x (1000^5 ) "quadrillion"
  | x >= 1000^4  = separate x (1000^4 ) "trillion"
  | x >= 1000^3  = separate x (1000^3 ) "billion"
  | x >= 1000^2  = separate x (1000^2 ) "million"
  | x >= 1000^1  = separate x (1000^1 ) "thousand"
  | x >= 100 = sayNumber (x `div` 100) ++ " hundred" ++
               case x `mod` 100 of
                 0 -> ""
                 r -> " and " ++ sayNumber r
  | x >= 20 = sayNumber ((x `div` 10) * 10) ++
              case x `mod` 10 of
                0 -> ""
                r -> "-" ++ sayNumber r
  where
    separate x sep name = concat . intersperse " " $
                          [ sayNumber (x `div` sep)
                          , name
                          ] ++ case x `mod` sep of
                            0 -> []
                            r -> [sayNumber (x `mod` sep)]

val = sum . map (genericLength . filter (`elem` ['a'..'z']) . sayNumber)
      $ [1..1000]

main = print val
