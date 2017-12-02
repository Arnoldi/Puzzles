import Int_function_lib (toDigits, fromDigits)
import Data.List (delete)

divisibleby x y = (x `mod` y) == 0

find_fitting_number divisors remaining_digits = _ffn [] ([1,1] ++ divisors) remaining_digits [0,0] 
    where
        _ffn current_digits divisors remaining_digits current_substring -- current_digits: the so far still plausible digits of the entire number, divisors: list of divisors, remaining_digits: digits that can still be used to make the number pandigital, current_substring: last two digits of the previous sub string
        -- the goal is to find the digits that will be added to the two digits of the previous sub string and still allow the divisibility test to pass
            | remaining_digits == [] = [fromDigits current_digits] -- a number found
            | possible_digits == [] = [] -- no possible combination here
            | otherwise = branch [] possible_digits  -- call branch with the entire list of possible digits
            where
                branch res [] = res -- tail call optimisable pattern
                branch res l = branch (res ++ (_ffn next_digits (tail divisors) next_rem_digits next_substring)) (tail l) -- branch will call _ffn for every digit in the list l
                    where
                        next_digits = current_digits ++ [digit]
                        next_rem_digits = delete digit remaining_digits
                        next_substring = (tail current_substring) ++ [digit]
                        digit = head l
                num = 10 * (fromDigits current_substring)
                test_divisibility = (`divisibleby` (head divisors))
                possible_digits = (filter (test_divisibility . (+num))) $ remaining_digits


test = sum $ find_fitting_number [1,2,3,5,7,11,13,17] [1,2,3,4,5,6,7,8,9,0]