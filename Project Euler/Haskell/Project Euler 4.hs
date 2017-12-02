import Int_function_lib (check_palindrome)

res = maximum . (filter check_palindrome) $ [n*m | n <- [100..1999], m <- [100..n]]

main = putStrLn . show $ res 