import Int_function_lib (fromDigits, toDigits)

res lines_ = let numbers = map (\s -> read s :: Integer) lines_
              in fromDigits . (take 10) . toDigits . sum $ numbers
        
res_from_file file = do
    content <- readFile file
    return (res . lines $ content)

main = res_from_file $ "Project Euler 13.txt"