import Data.Function (fix)
import Data.List (foldl1')

recurringFraction :: Integral a => a -> [[a]]
recurringFraction n = [nonrecurr, recurr]
    where
        recurr = (map fst) . (drop 1) . (dropWhile (/= lastelem)) $ total
        nonrecurr = (map fst) . (takeWhile (/= lastelem)) $ total
        lastelem = last total
        total = fraction 1 n []
            where 
                fraction 0 d acc = acc ++ [(0, 0)]
                fraction n d acc =
                    let
                        current_rem = (10*n) `rem` d
                        current_div = (10*n) `div` d
                    in if ((elem (current_div, current_rem)) $ acc)
                       then acc ++ [(current_div, current_rem)]
                       else (fraction current_rem d (acc ++ [(current_div, current_rem)]))


res = fst . (foldl1' (\x y -> if (snd x > snd y) then x else y)) . (zip num) . (map (length . (!!1) . recurringFraction)) $ num
    where num = [1..1000-1]
    
main = putStrLn . show $ res