import Int_function_lib (isPrimeMemoized, primes, divisiblebylist)
import Data.List (foldr1, foldl1')

candidates = (filter (not . isPrimeMemoized)) $ [3,5..]

test_goldbach n = (foldr1 (||)) . (map isPrimeMemoized) . (takeWhile (>0)) $ [n-a*a*2 | a<-[1..]]
res = head . (filter (not . test_goldbach)) $ candidates

main = putStrLn . show $ res