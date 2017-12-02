truncated_mult :: Integral a => a -> a -> a
truncated_mult a b = mod (a * b) (10 ^ 10)

fast_power :: Integral a => a -> a -> a
fast_power n e
    | e == 0 = 1
    | e == 1 = n
    | even e = (\ x -> x `truncated_mult` x) $ fast_power n (e `div` 2)
    | odd e = n `truncated_mult` (fast_power n (e - 1))
    | otherwise = 0

test = mod (sum $ map (\n -> fast_power n n) [1..1000]) (10 ^ 10)
test3 = (mod (sum (map (\n -> n ^ n) [1..1000])) (10 ^ 10))

res = mod (sum $ map (\n -> n ^ n) [1..1000]) (10 ^ 10)


main = putStrLn . show $ res