import Int_function_lib (num_to_lit)


main = putStrLn . show . length . concat . (map ((filter (\c -> c/=' ' && c/='-')) . num_to_lit)) $ [1..1000] -- convert numbers, remove white spaces and hyphens, concat all numbers and get the length of the entire string.
