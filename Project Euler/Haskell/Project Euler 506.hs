import Int_function_lib
import Data.List


clock = [1,2,3,4,3,2] ++ clock

find_repeating_steps l = _test 1 l
    where
        _test 5000 _ = 0
        _test n l
            | first == next = n
            | otherwise = _test (n+1) l
            where
                first = take (2*n) l
                next = take (2*n) (drop n l)

seq_n = _seq_n 1 0 0 clock
    where
        _seq_n n s k l
            | n > s = _seq_n n (s+d) (10*k + d) (tail l)
            | otherwise = [k] ++ (_seq_n (n+1) 0 0 l)
            where
                d = head l


cut n = n `rem` 123454321

test = (`rem` 123454321) . sum . (take 1000) $ seq_n

test1 = length . nub . (map (`rem` 123454321)) . (take 10000) $ seq_n

sn = scanl1 (+) seq_n

last_n_digits n = (map ((`rem` 10) . (`div` (10^(n-1))))) $ seq_n

groupby n l = [fromDigits . (take n) $ l] ++ (groupby n (drop n l))


b = 4

test2 l = groupby (find_repeating_steps (drop (15*b) l)) l

test3 = test2 $ last_n_digits b

test4 = (map (\n -> fromDigits . (take 15) . (drop (15*n)) . last_n_digits $ n)) $ [1..]

test5 = zipWith (rem) (drop 15 sn) sn