import Int_function_lib

import Data.List

find_conssumPrimes n p = fcsp n 0 1 0 [] p
    where
        fcsp n csum cmax len l p
            | (csum + (head p)) >= n = res ++ if len > new_cmax
                                                                 then fcsp n (csum - (head l)) new_cmax (len-1) (tail l) p
                                                                 else []
            | otherwise = (fcsp n (csum + (head p)) cmax (len+1) (l ++ [head p]) (tail p))
            where
                res = (travel_list n len (tail l)) 
                new_cmax = max cmax (if res /= []
                                                     then (snd . head $ res)
                                                     else 1)
                travel_list _ _ [] = []
                travel_list s len l
                    | len < cmax = []
                    | isPrime csum = [(csum, len)]
                    | otherwise = travel_list (csum - (head l)) (len-1) (tail l)

test2 = find_conssumPrimes 1000000 primes
test = fst . last . (sortBy (\a b -> compare (snd a) (snd b))) $ find_conssumPrimes 1000000 primes
