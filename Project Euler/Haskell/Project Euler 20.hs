import Int_function_lib (toDigits, factorial)

res = sum . toDigits $ factorial 100

main = putStrLn . show $ res
