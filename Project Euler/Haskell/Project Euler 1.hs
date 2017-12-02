import Data.List (union)

k = 1000-1

nth n = map (n*) [1..(k `div` n)]

third = nth 3
fifth = nth 5

res = sum $ union third fifth

main = putStrLn . show $ res