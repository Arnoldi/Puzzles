import Int_function_lib (toDigits, fromDigits, create_tuple_orig_and_transf, compare_tuple_pair)
import Data.List

numbers = [2..6*9^5]
digit_fifth_power_sum :: [Int] -> Int
digit_fifth_power_sum = sum . (map (^5))
res = sum . (map fst) . (filter compare_tuple_pair) . (map (create_tuple_orig_and_transf (digit_fifth_power_sum . toDigits))) $ numbers


numbers6Digits = [[f,e,d,c,b,a] | f <- [0..9], e <- [f..9], d <- [e..9], c <- [d..9], b <- [c..9], a <- [b..9], a>1]

res2 = sum . (map fst) . filterNumbers . findTransformed $ candidates
    where candidates = map digit_fifth_power_sum numbers6Digits
          findTransformed = (map (create_tuple_orig_and_transf (digit_fifth_power_sum . toDigits)))
          filterNumbers = (filter compare_tuple_pair)

main = putStrLn . show $ res2
