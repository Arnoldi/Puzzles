import Control.Monad (liftM)
import Data.List (sort)
import Data.Char (ord)

replace ',' = ' '
replace '"' = ' '
replace c = c

extract_words :: String -> [String]
extract_words = words . (map replace)

char_to_num c = (ord c) - (ord 'A' - 1)
word_to_num = sum . (map char_to_num)

mult_by_pos :: [Int] -> [Int]
mult_by_pos l = zipWith (*) l [1..length l]

res = sum . mult_by_pos . (map word_to_num) . sort . extract_words

res_from_file file = do
    content <- readFile file
    return (res $ content)

main = res_from_file $ "Project Euler 22.txt"