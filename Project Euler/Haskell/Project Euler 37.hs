import Int_function_lib (ilogk, toDigits, fromDigits, primes, isPrimeMemoized)
import Data.List ((\\),nub)

candidates = start_digits ++ (map_digits start_digits)
    where   start_digits = [[2],[3],[5],[7],[9]]
            map_digits l = next ++ map_digits next
                where   next = (concat . (map add_digit) $ l)
                        add_digit l = (map ((l++) . to_list) $ digits)
                            where   digits = [1,3,7,9]
                                    to_list = (:[])

right_truncable_prime n
    | n < 10 = isPrimeMemoized n
    | isPrimeMemoized n = right_truncable_prime (n `div` 10)
    | otherwise = False

left_truncable_prime n = _trunc n (10^(ilogk 10 n))
    where
        _trunc n b
            | n < 10 = isPrimeMemoized n
            | isPrimeMemoized n = _trunc (n `rem` b) (b `div ` 10)
            | otherwise = False

res = sum . (take 11) . (filter right_truncable_prime) . (filter left_truncable_prime) . (dropWhile (<10)) . (filter isPrimeMemoized) . (map fromDigits) $ candidates

main = putStrLn . show $ res