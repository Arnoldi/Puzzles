import Int_function_lib (toDigits)

concat_int = concat . (map toDigits) $ [1..]

d n = concat_int !! (n-1)

test = (d 1)*(d 10)*(d 100)*(d 1000)*(d 10000)*(d 100000)*(d 1000000)