import Int_function_lib (toDigits, fromDigits, create_tuple_orig_and_transf, compare_tuple_pair)
import Data.List ((\\), intersect)

candidates = [(a,b) | a <- [10..99], b <- [10..99], not_round a, not_round b, a < b, not_twin_digit a, not_twin_digit b]
    where
        not_twin_digit n = n `rem` 10 /= n `div` 10
        not_round n = n `rem` 10 /= 0

strike_digit t = (fromDigits $ (\\) num striked_digit, fromDigits $ (\\) denom striked_digit)
    where 
        striked_digit = if common == [] then [] else [head common]
        common = intersect num denom
        num = toDigits $ fst t
        denom = toDigits $ snd t

simplify t = (norm $ fst t, norm $ snd t)
    where norm = (`div` (gcd (fst t) (snd t)))

tuple_map f t = (f $ fst t, f $ snd t)

find_strikable_numbers = (filter (not . compare_tuple_pair)) . (map (create_tuple_orig_and_transf strike_digit))
find_numbers_with_identical_simplification = filter (compare_tuple_pair . (tuple_map simplify))
get_denom = snd
multiply_numbers = simplify . (tuple_map product) . unzip

res = get_denom . multiply_numbers . (map fst) . find_numbers_with_identical_simplification . find_strikable_numbers $ candidates

main = putStrLn . show $ res
