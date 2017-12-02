import Int_function_lib (toDigits)

compare_lists :: (Num a, Ord a) => [a] -> [a] -> Bool
compare_lists [] [] = True
compare_lists [] _ = False
compare_lists _ [] = False
compare_lists l1 l2 = (foldl (&&) True) . (map (>=0)) $ zipWith (-) l1 l2

is_increasing :: (Num a, Ord a) => [a] -> Bool
is_increasing [] = True
is_increasing digits = compare_lists (tail digits) (init digits)

is_decreasing :: (Num a, Ord a) => [a] -> Bool
is_decreasing [] = True
is_decreasing digits = compare_lists (init digits) (tail digits)

is_bouncy :: Integer -> Bool
is_bouncy number = let digits = toDigits number
                   in not ((is_increasing digits) || (is_decreasing digits))

count_true = (scanl (\(t,s) b -> (t + if b then 1 else 0, s+1)) (0,0))
test x = (take 1) . (dropWhile (\(t,s) -> (100*t `div` s < x))) . (drop 1) . count_true . (map (is_bouncy)) $ [1..]
