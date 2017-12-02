import Int_function_lib (isPrimeMemoized, primeFactorsMemoized, primes)
import Data.List (length, takeWhile, zip, zipWith, concat, replicate)

spiral_corners = tail . (scanl (+) 1) . concat $ [replicate 4 a | a <- [2,4..]]
count_corners = zip3 spiral_corners [2..] (concat $ [replicate 4 a | a <- [3,5..]])
filtered_primes = (filter (\(t,_,_) -> isPrimeMemoized t)) $ count_corners
ratio_primes = zip side_length $ zipWith (/) count_primes count_corners
    where unzipped_list = unzip3 $ filtered_primes
          count_primes = [1..]
          count_corners = (\(_,t,_) -> t) unzipped_list
          side_length = (\(_,_,t) -> t) unzipped_list
          

res = fst . last . (takeWhile ((>=0.10) . snd)) $ ratio_primes
main = putStrLn . show $ res