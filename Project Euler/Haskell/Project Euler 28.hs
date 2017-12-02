
-- diagonal_value 0 = 1
-- diagonal_value n = 16*n*n + 4*n + 4

res = 1 + (16*di*di*di + 30*di*di + 26*di) `div` 3
    where di = 1001 `div` 2

main = putStrLn . show $ res


--43 44 45 46 47 48 49
--42 21 22 23 24 25 26
--41 20 07 08 09 10 27
--40 19 06 01 02 11 28
--39 18 05 04 03 12 29
--38 17 16 15 14 13 30
--37 36 35 34 33 32 31