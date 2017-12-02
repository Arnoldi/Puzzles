import Int_function_lib (sum_proper_divisors)
import Data.List
import Data.Array

n = 28123 -- 20161

abundant n = (sum_proper_divisors n > n)

abundantArray = listArray(1,n) $ map abundant [1..]
isAbundant = (abundantArray !)
abundantNumbers = filter (abundantArray !) [1..n]

diffs m = (map (m-))
rests m = (diffs m) . (takeWhile (<= m `div` 2)) $ abundantNumbers

is_sum_of_2_abundant_numbers2 = (any isAbundant) . rests

res = sum . (filter (not . is_sum_of_2_abundant_numbers2)) $ [1..n]

main = print . show $ res