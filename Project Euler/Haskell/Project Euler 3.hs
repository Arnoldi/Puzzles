import Int_function_lib (primeFactors)

res = last . primeFactors $ 600851475143

main = putStrLn . show $ res