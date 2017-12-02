import Data.Array
import Data.List
import Data.Ord (comparing)
import Debug.Trace


convertArrayToList = assocs

memoCollatzDepth n = memoArray
    where   memoArray = listArray(1,n) $ 0 : map collatzDepth [2..n]
            collatzDepth m = 1 + if (next_n > n)
                                  then collatzDepth next_n
                                  else memoArray ! next_n
                where next_n = if even m
                               then m `div` 2
                               else 3*m+1

main = putStrLn . show . fst . maximumBy (comparing snd) . convertArrayToList $ memoCollatzDepth 1000000
