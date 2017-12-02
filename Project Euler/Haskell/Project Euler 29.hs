import Data.List (nub)

numbers = [a^b | a <- [2..100], b <- [2..100]]

res = length . nub $ numbers

main = putStrLn . show $ res
