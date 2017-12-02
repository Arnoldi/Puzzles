import Int_function_lib (number_divisors)
import Data.List (foldr1)


triangle_numbers = scanl (+) 1 [2..]

n = 500

main = putStrLn . show . head . (dropWhile (\m -> n > number_divisors m)) $ triangle_numbers