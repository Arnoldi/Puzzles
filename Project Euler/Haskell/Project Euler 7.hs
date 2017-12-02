import Int_function_lib (primes)

res = primes !! 10000

main = putStrLn . show $ res