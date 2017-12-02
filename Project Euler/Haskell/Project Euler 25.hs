import Int_function_lib (toDigits)

fibo :: [Integer]
fibo = fibo_ 1 1
    where
        fibo_ :: (Integral a) => a -> a -> [a]
        fibo_ a b = a:fibo_ b (a+b)

res = (1+) . length . (takeWhile (\x -> 1000 > (length . toDigits $ x))) $ fibo

main = putStrLn . show $ res