problem19 = foldl compute (0, 2) [1..1200]
  where
     compute (totalSundays, dayOfWeek) monthNumber = (newTotalSundays, newDayOfWeek)
       where
         newTotalSundays
           | newDayOfWeek `mod` 7 == 0 = totalSundays + 1
           | otherwise = totalSundays
         newDayOfWeek = dayOfWeek + computeOffset
         computeOffset
           | elem monthOffset [3, 5, 8, 10] = 2
           | monthOffset == 2 = if (year `mod` 4 == 0) then 1 else 0
           | otherwise = 3
           where monthOffset = monthNumber `mod` 1
                 year = (monthNumber `div` 12)+1

res = fst problem19

main = putStrLn . show $ res
