-- Naive non-general solution

normalYear = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
leapYear   = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
monthLengths = concat $ normalYear :
               cycle [normalYear, normalYear, normalYear, leapYear]

firstDays = drop 12 $ map (`mod` 7) $ scanl (+) 0 monthLengths

val = length $ filter (==6) $ take 1200 firstDays
