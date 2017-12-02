import Int_function_lib (is_panDigitalI, ilogk, primes, isPrime)

check_n_pandigital n = is_panDigitalI ((ilogk 10 n)+1) n

numbers = _numbers 7654321
    where
        _numbers 0 = []
        _numbers n = [n] ++ _numbers (n-1)

test = head . (filter check_n_pandigital) . (filter isPrime) $ numbers