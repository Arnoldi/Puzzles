import Int_function_lib (toDigits, primes, ilogk)

import Data.List


num_digits = length . toDigits
primes_of_length n = (takeWhile ((<=n) . num_digits)) primes
