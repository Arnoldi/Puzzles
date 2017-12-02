import Int_function_lib (create_tuple_orig_and_transf)
import Data.List (maximumBy)

list_combinations p = [[a,b,c] | a <- [1..p], b <- [a..p], c <- [p-a-b], c > 0, a^2 + b^2 == c^2]

sortFun (a,_) (b,_) = if a > b then GT else if a < b then LT else EQ

test = snd . (maximumBy sortFun) . (filter ((>0) . fst)) . (map (create_tuple_orig_and_transf (length . list_combinations))) $ [1..1000]