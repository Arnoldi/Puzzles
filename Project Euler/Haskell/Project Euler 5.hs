import Int_function_lib (check_palindrome, ilogk, primes)

smallest_multiple_below n = product $ zipWith (^) primes_to (map (\k -> ilogk k n) primes_to)
    where primes_to = (takeWhile (<= n)) $ primes

res = smallest_multiple_below 20

main = putStrLn . show $ res