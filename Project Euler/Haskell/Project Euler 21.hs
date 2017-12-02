import Int_function_lib (sum_proper_divisors)

amicable_pair_half 1 = False
amicable_pair_half n = (m == n && n /= l)
                      where l = sum_proper_divisors n
                            m = sum_proper_divisors l

res = sum . (filter amicable_pair_half) $ [1..10000]

main = putStrLn . show $ res