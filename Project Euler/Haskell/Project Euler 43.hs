import Int_function_lib (toDigits, fromDigits, sliding_window, is_panDigitalI, primes, divisibleby)
import Data.List (foldr1)

create_substrings l = (map fromDigits) . (sliding_window 3) . toDigits $ l
check_divisibility_prime l = (foldr1 (&&)) $ zipWith divisibleby l primes

test = (filter (check_divisibility_prime . create_substrings)) . (filter (is_panDigitalI 10)) $ [1023456789..9876543210]