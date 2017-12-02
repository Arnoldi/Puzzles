import Int_function_lib (toDigits)

main = putStrLn . show . sum . toDigits $ 2^1000

