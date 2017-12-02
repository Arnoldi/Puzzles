import Int_function_lib (primes)


--sum_below :: Integer -> [Integer] -> Integer
sum_below n = sum . (takeWhile (<n))

res = (sum_below (2*10^6)) $ primes

main = putStrLn . show $ res