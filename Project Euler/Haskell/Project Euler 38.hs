import Int_function_lib (ilogk, toDigits, fromDigits, primes, isPrime, is_panDigitalI, is_panDigitalL)
import Data.List (length)

create_concat_product2 k n = _con [] k n 1
    where
        _con l k n f
            | len == k = l
            | len > k = []
            | otherwise = _con (l ++ (toDigits (n*f))) k n (f+1)
            where len = length l

res = maximum . (map fromDigits) . (filter (is_panDigitalL 9)) . (filter (/=[])) . (map (create_concat_product2 9)) $ [1..10000]

main = putStrLn . show $ res