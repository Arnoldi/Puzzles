import Int_function_lib (toDigits, fromDigits, factorial, create_tuple_orig_and_transf, compare_tuple_pair)
import Data.List (nub, sort)
import Data.Char (digitToInt)

digit_factorial_sum :: Integer -> Integer
digit_factorial_sum = sum . (map factorial) . toDigits

res = sum . (map snd) . (filter compare_tuple_pair) . (map (create_tuple_orig_and_transf digit_factorial_sum)) $ [3..(factorial 9)*7]

main = putStrLn . show $ res