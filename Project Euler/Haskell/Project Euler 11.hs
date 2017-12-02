import Int_function_lib (string_to_number_matrix)
--import System.Environment
import Data.List (transpose)
import Control.Monad

chunk :: Int -> [a] -> [[a]]
chunk n l  
  | length chunk' < n = []
  | otherwise = (chunk' : chunk n (tail l))
  where chunk' = take n l

find_in_rows_of_matrix :: Eq a => ([b] -> b) -> ([a] -> b) -> Int -> [[a]] -> b
find_in_rows_of_matrix f g n = f . (map (f . (map g))) . (filter (/=[])) . (map (chunk n))

diagonal_tl_br :: [[a]] -> [a]
diagonal_tl_br l = let lenx = (length l)
                       leny = (length (l !! 0))
                   in diagonal_tl_br_ 0 0 lenx leny l
diagonal_tl_br_ x y a b l
   | x >= a = []
   | y >= b = []
   | otherwise = ((l !! x) !! y) :(diagonal_tl_br_ (x+1) (y+1) a b l)

diagonals_tl_br :: [[a]] -> [[a]]
diagonals_tl_br l = let lenx = (length l)
                        leny = (length (l !! 0))
                    in diagonals_tl_br_ 0 (leny-1) lenx leny l
                        where
                            diagonals_tl_br_ 0 0 a b l = (diagonal_tl_br_ 0 0 a b l):diagonals_tl_br_ 1 0 a b l
                            diagonals_tl_br_ 0 y a b l = (diagonal_tl_br_ 0 y a b l):diagonals_tl_br_ 0 (y-1) a b l
                            diagonals_tl_br_ x 0 a b l
                                | x < a = (diagonal_tl_br_ x 0 a b l):diagonals_tl_br_ (x+1) 0 a b l
                                | otherwise = []
                            diagonals_tl_br_ x y a b l = []

res numbers =
    let from_rows            = (find_in_rows_of_matrix maximum product 4) $ numbers
        from_columns         = (find_in_rows_of_matrix maximum product 4) $ transpose numbers
        from_diagonals_tl_br = (find_in_rows_of_matrix maximum product 4) . diagonals_tl_br $ numbers
        from_diagonals_bl_tr = (find_in_rows_of_matrix maximum product 4) . diagonals_tl_br $ reverse numbers
    in maximum [from_rows, from_columns, from_diagonals_tl_br, from_diagonals_bl_tr]


res_from_file file = do
    content <- readFile file
    return (res . string_to_number_matrix $ content)

main = res_from_file $ "Project Euler 11.txt"
