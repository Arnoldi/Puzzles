--import Int_function_lib (toDigits, fromDigits)

import Data.List (foldl1', elemIndex)
import Data.Char (digitToInt)

toDigits = map (fromIntegral . digitToInt) . show
fromDigits = foldl1' (\a b -> a*10 + b)

count x list = fromIntegral . length $ filter (== x) list

count_digit number digit = count digit (toDigits number)

num_iter :: Integer -> Integer
num_iter number = fromDigits $ map (count_digit number) [0..9]

--index_first_diff l1 l2 = (elemIndex False) . (map (\t -> (fst t) == (snd t))) $ zip l1 l2
--num_iter2 :: Integer -> Integer
--num_iter2 number = let new_number = toDigits number
--                       index = index_first_diff new_number number
--                   in if (index == Nothing)
--                      then [number]
--                      else number:(num_iter2 . fromDigits $ append (take index )
                      
digit_iter [] _ _ = []
digit_iter _ _ 10 = []
digit_iter digitlist number digit =
    let d_new = count_digit number digit
        d_old = head digitlist
    in d_new: if (d_old == d_new)
              then digit_iter (tail digitlist) number (digit + 1)
              else tail digitlist

--num_iter2 :: Integer -> Integer
num_iter2 number = fromDigits $ let digits = toDigits number
                                    digitlist = digits ++ (take (max 0 (10 - length digits)) (repeat 0))
                                in digit_iter digitlist number 0

--if (count_digit number digit)

find_fixpoint f x = let new_x = f x
                    in if (new_x == x)
                       then [new_x]
                       else x:(find_fixpoint f new_x)

check n = find_fixpoint num_iter2 n

main = print . (take 10) $ check 1

--test = toDigits 10