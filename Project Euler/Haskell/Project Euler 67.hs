--import System.Environment
import Int_function_lib (string_to_number_matrix)
import Control.Monad (liftM)

maxbranch :: Integral a => [[a]] -> a
maxbranch l = maximum $ maxbranch_ (head l) (tail l)
    where
        maxbranch_ :: Integral a => [a] -> [[a]] -> [a]
        maxbranch_ accumlist [] = accumlist
        maxbranch_ accumlist l = 
            let current_row = l !! 0
                accumlist_branch = zip ([head accumlist] ++ accumlist) (accumlist ++ [last accumlist])
                max_branch = map (\a -> max (fst a) (snd a)) accumlist_branch
                next_accumlist = zipWith (+) current_row max_branch
            in maxbranch_ next_accumlist (drop 1 l) 


res = maxbranch

res_from_file file = do
    content <- readFile file
    return (res . string_to_number_matrix $ content)

main = res_from_file $ "Project Euler 67.txt"