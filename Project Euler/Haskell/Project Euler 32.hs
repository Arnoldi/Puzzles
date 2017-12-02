import Int_function_lib (toDigits, fromDigits, is_panDigitalL, is_panDigitalI)
import Data.List (nub, sort, delete, (\\), splitAt )
import Data.Char (digitToInt)

--duplicate_digits n = (length . nub $ m) /= (length m)
--    where m = toDigits n
--duplicate l = (length . nub $ l) /= (length l)

--cartProd xs ys = [(x,y) | x <- xs, y <- ys]

add_digit l = map ((l++) . to_list) $ digits \\ l
    where digits = [1..9]
          to_list = (:[])

panDigitNumbers 0 = [[]]
panDigitNumbers length = concatMap add_digit (panDigitNumbers (length - 1))

numbers_all = (map (splitAt 1) numbers) ++ (map (splitAt 2) numbers)
    where get_numbers = panDigitNumbers -- (filter ((0/=) . head)) $
          numbers = get_numbers 4 ++ get_numbers 5 ++ get_numbers 6

filter_numbers (x:xs,_) = not (x == 1 && xs == []) -- && (x/=0) && (y/=0)
numbers2 = (filter (filter_numbers)) $ numbers_all

add_product (t,s) = (toDigits(new_t * new_s),t,s)
    where new_t = fromDigits t
          new_s = fromDigits s
numbers3 = (map add_product) $ numbers2
numbers = numbers3

pack (prod,t,s) = (fromDigits prod, prod ++ t ++ s)
res =  sum . nub . (map fst) . (map (\t -> (fst t, fromDigits . snd $ t))) . (filter ((is_panDigitalL 9) . snd)) . (map (pack . add_product)) . (filter (filter_numbers)) $ numbers_all

numbersa = [(a*b,a,b) | a <- [2..98], b <- [102..9876], a*b <= 98*765 ]

res2 =  sum . nub . (map fst) . (map (\t -> (fst t, fromDigits . snd $ t))) . (filter ((is_panDigitalL 9) . snd)) . (map pack) $ numbers
    where numbers = [(a*b,a,b) | a <- [2..98], b <- [102..9876], a*b <= 98*765 ]
          pack (prod,t,s) = (prod, toDigits prod ++ toDigits t ++ toDigits s)

res3 =  sum . nub . (map fst) . (map (\t -> (fst t, fromDigits . snd $ t))) . (filter ((is_panDigitalL 9) . snd)) . (map pack) $ numbers
    where numbers = [[fromDigits [a],fromDigits [b,c,d,e],(fromDigits [a]) * (fromDigits [b,c,d,e])] | a <- [1..9], b <- [1..9], c <- [0..9], d <- [0..9], e <- [0..9], a /= b, a /= c, a /= d, a /= e, b /= c, b /= d, b /= e, c /= d, c /= e, d /= e]
          pack l  = (last l, concat . (map toDigits) $ l)

main = putStrLn . show $ res
