import Int_function_lib (toDigits)
import Data.List (iterate)
import Data.Ratio ((%), numerator, denominator)


num_digits = length . toDigits
digits_nom_denom r = (num_digits . numerator $ r) > (num_digits . denominator $ r)

fractions_list = iterate (\r -> 1+1/(1+r)) 1

res = length . (filter digits_nom_denom) . (take 1000) $ fractions_list

main = putStrLn . show $ res

--comb_fun r = 1 + (1/(1 + r))
--fractions_list = iterate ((1+).(1/).(1+)) 1