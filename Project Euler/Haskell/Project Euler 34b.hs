import Int_function_lib (toDigits, fromDigits, factorial, create_tuple_orig_and_transf, compare_tuple_pair, combinationsOf)
import Data.List (nub, sort, zip, zipWith)
import Data.Char (digitToInt)
import Data.Array (listArray, (!))

list_factorial = listArray(0,9) (map factorial [0..9])
memoized_factorial n = list_factorial ! n

digit_factorial_sum :: Integer -> Integer
digit_factorial_sum = sum . (map memoized_factorial) . toDigits

combs = combinationsOf 9 [0..9]
nums = (map (sum . (map memoized_factorial))) $ combinationsOf 9 [0..9]
nums2 = (map digit_factorial_sum) $ nums
res = sum . nub . (map fst) . (filter (\(t,s) -> t == s)) $ zip nums nums2
main = putStrLn . show $ res