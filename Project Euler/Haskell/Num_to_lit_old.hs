module Num_to_lit_old (num_to_lit) where

num_to_lit :: Integer -> String
num_to_lit 0 = "zero"
num_to_lit n = reverse . (dropWhile (==' ')) . reverse . num_to_lit_ $ n -- to remove trailing white spaces after thousand, million etc.
    where
        num_to_lit_ 0 = ""
        num_to_lit_ 1 = "one"
        num_to_lit_ 2 = "two"
        num_to_lit_ 3 = "three"
        num_to_lit_ 4 = "four"
        num_to_lit_ 5 = "five"
        num_to_lit_ 6 = "six"
        num_to_lit_ 7 = "seven"
        num_to_lit_ 8 = "eight"
        num_to_lit_ 9 = "nine"
        num_to_lit_ 10 = "ten"
        num_to_lit_ 11 = "eleven"
        num_to_lit_ 12 = "twelve"
        num_to_lit_ 13 = "thirteen"
        num_to_lit_ 14 = "fourteen"
        num_to_lit_ 15 = "fifteen"
        num_to_lit_ 16 = "sixteen"
        num_to_lit_ 17 = "seventeen"
        num_to_lit_ 18 = "eighteen"
        num_to_lit_ 19 = "nineteen"
        num_to_lit_ 20 = "twenty"
        num_to_lit_ 30 = "thirty"
        num_to_lit_ 40 = "forty"
        num_to_lit_ 50 = "fifty"
        num_to_lit_ 60 = "sixty"
        num_to_lit_ 70 = "seventy"
        num_to_lit_ 80 = "eighty"
        num_to_lit_ 90 = "ninety"
        num_to_lit_ n
            | n < 0 = "minus " ++ num_to_lit_ (-n)
            | n < 100 = let ns = n `rem` 10
                        in num_to_lit_ (n - ns) ++ "-" ++ num_to_lit_ ns
            | n < 1000 && n `rem` 100 == 0 = num_to_lit_ (n `div` 100) ++ " hundred"
            | n < 1000 = let ns = n `rem` 100
                         in num_to_lit_ (n - ns) ++ " and " ++ num_to_lit_ ns
            | n < 10^6 = let ns = n `rem` 1000 
                         in num_to_lit_ ((n - ns) `div` 1000) ++ " thousand " ++ num_to_lit_ ns
            | n < 10^9 = let ns = n `rem` 10^6
                         in num_to_lit_ ((n - ns) `div` 10^6) ++ " million " ++ num_to_lit_ ns
            | n < 10^12 = let ns = n `rem` 10^9
                          in num_to_lit_ ((n - ns) `div` 10^9) ++ " billion " ++ num_to_lit_ ns
            | n < 10^15 = let ns = n `rem` 10^12
                          in num_to_lit_ ((n - ns) `div` 10^12) ++ " trillion " ++ num_to_lit_ ns
            | n < 10^18 = let ns = n `rem` 10^15
                          in num_to_lit_ ((n - ns) `div` 10^15) ++ " quadrillion " ++ num_to_lit_ ns
            | n < 10^21 = let ns = n `rem` 10^18
                          in num_to_lit_ ((n - ns) `div` 10^18) ++ " quintillion " ++ num_to_lit_ ns
            | n < 10^24 = let ns = n `rem` 10^21
                          in num_to_lit_ ((n - ns) `div` 10^21) ++ " sextillion " ++ num_to_lit_ ns
            | n < 10^27 = let ns = n `rem` 10^21
                          in num_to_lit_ ((n - ns) `div` 10^21) ++ " sextillion " ++ num_to_lit_ ns
            | otherwise = "number to large, not implemented"