square x = x*x

sum_of_squares = sum . (map square)
square_of_sums = square . sum

res = (\l -> square_of_sums l - sum_of_squares l) [1..100]

main = putStrLn . show $ res