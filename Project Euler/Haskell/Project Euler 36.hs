import Int_function_lib (check_palindromeL, convert_to_binary, palindromes)


res = sum . (filter (check_palindromeL . convert_to_binary)) . (takeWhile (<1000000)) $ palindromes

main = putStrLn . show $ res