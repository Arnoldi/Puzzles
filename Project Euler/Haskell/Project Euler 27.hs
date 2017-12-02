import Int_function_lib (isPrimeMemoized)
import Data.List
import Data.Array

numPrimes a b = length $ takeWhile isPrimeMemoized [n^2 + a*n + b | n <- [0..]]

s = 999
coefList :: [(Int, Int, Int)]
coefList = [(a,b,numPrimes a b) | a <- [-s..s], b <- [3..s], odd a, isPrimeMemoized b]

sortFun (_,_,a) (_,_,b) = if   a > b
                          then GT
                          else if   a < b
                               then LT
                               else EQ

res = (\(x,y,_) -> x*y) . (maximumBy sortFun) . (filter (\(_,_,t) -> t>39)) $ coefList

test4 = (\(x,y,_) -> x*y) . (maximumBy sortFun) $ coefList
test2 = length coefList
test3 = length . (filter (\(_,_,t) -> t>39)) $ coefList

main = putStrLn . show $ res