
fibo :: [Integer]
fibo = fibo_iter 0 1
    where
        fibo_iter a b = a:fibo_iter b (a+b)

res = sum . (filter even) . (takeWhile (<4*10^6)) $ fibo

main = putStrLn . show $ res