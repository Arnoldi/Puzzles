import Int_function_lib (toDigits, fromDigits, isPrimeMemoized, primes)
import Data.List (all)

rotate_list l = (tail l) ++ [head l]

all_rotations :: [a] -> [[a]]
all_rotations l = _rot (length l) l
    where
        _rot _ [] = []
        _rot 0 _ = []
        _rot n l = [l] ++ (_rot (n-1) (rotate_list l))

--all_rotations_num :: Int -> [Int]
all_rotations_num l = (map fromDigits) . all_rotations $ l

limit = 1000000

candidates = (++ [[2],[5]]) . (filter (all (/=5))) . (filter (all odd)) . (map toDigits) . (takeWhile (<=limit)) $ primes
res = length . (filter (all isPrimeMemoized)) . (map all_rotations_num) $ candidates 
-- 
main = putStrLn . show $ res
