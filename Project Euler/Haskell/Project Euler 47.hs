import Int_function_lib (isqrt, primes, divisiblebylist, primeFactors, number_divisors, numPrimeFactors, primeFactorsMemoized)
import Data.List (group)

candidates m = (filter (\n -> (numPrimeFactors n) >= m)) $ [1..]

uniquePrimeFactors = length . group . primeFactorsMemoized

--listUniquePrimeFactors m = (map (\x -> (x, uniquePrimeFactors x))) $ [1..]
listUniquePrimeFactors m = (filter (\t -> (snd t) == 4)) . (map (\x -> (x, uniquePrimeFactors x))) $ candidates m


consecutivePrimeFactors numConsecutive _numPrimeFactors = (_consPrimFac (listUniquePrimeFactors _numPrimeFactors) 0 0) - numConsecutive
    where 
        _consPrimFac :: [(Int,Int)] -> Int -> Int -> Int
        _consPrimFac current count last_num
          | count == numConsecutive = current_num - numConsecutive + 1
          | otherwise = _consPrimFac (tail current)
                                     (if ((last_num+1) == current_num)
                                      then (count+1)
                                      else 1)
                                      current_num
               where current_num = fst (head current)

res :: Int
res = consecutivePrimeFactors 4 4

main = putStrLn . show $ res