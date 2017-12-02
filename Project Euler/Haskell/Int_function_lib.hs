module Int_function_lib (ilogk, divisibleby, divisiblebylist, filterdivby, isqrt, ifast_power, toDigits, fromDigits, check_palindrome, check_palindromeL, primes, wheelSieve, isPrime, isPrimeMemoized, primeFactors, sliding_window, divisors, number_divisors, num_to_lit, string_to_number_matrix, factorial, sum_proper_divisors, create_tuple_orig_and_transf, compare_tuple_pair, is_panDigitalI, is_panDigitalL, remove_factor, combinationsOf, convert_to_binary, palindromes, numPrimeFactors, primeFactorsMemoized) where

import Data.Char (digitToInt)
import Data.List (nub, foldl', foldl1, foldl1', foldr1)
import Data.Numbers.Primes
import Num_to_lit
import Data.Array (listArray, (!))

divisibleby :: (Integral a) => a -> a -> Bool
divisibleby x y = (x `rem` y) == 0

divisiblebylist :: (Integral a) => a -> [a] -> Bool
divisiblebylist x l = foldr1 (||) $ map (\y -> divisibleby x y) l

filterdivby :: (Integral a) => a -> [a] -> [a]
filterdivby y = filter (\x -> divisibleby x y)

isqrt :: Integral a => a -> a
isqrt n = isqrt_ n n
    where
        isqrt_ x n
            | x2 >= x = x
            | otherwise = isqrt_ x2 n
            where x2 = (x + n `div` x) `div` 2

ifast_power :: Integral a => a -> a -> a
ifast_power n e
    | e == 0 = 1
    | e == 1 = n
    | even e = (\ x -> x * x) $ ifast_power n (e `div` 2)
    | odd e = n * (ifast_power n (e - 1))
    | otherwise = 0

toDigits :: (Integral int, Show int) => int -> [int]
toDigits = map (fromIntegral . digitToInt) . show

fromDigits :: Integral int => [int] -> int
fromDigits = foldl1' (\a b -> a*10 + b)

--max_from_list' :: (Integral a, Ord a) => [a] -> a
--max_from_list' = foldl1' (max)

check_palindrome :: Integer -> Bool
check_palindrome n = digits == reverse digits
    where digits = toDigits n

check_palindromeL :: Eq a => [a] -> Bool
check_palindromeL l = l == reverse l

list_sliding_entry :: [a] -> [[a]]
list_sliding_entry [] = []
list_sliding_entry l = l:(list_sliding_entry (tail l))

sliding_window :: Int -> [a] -> [[a]]
sliding_window n l = (filter (\l -> (length l) == n)) . (map (take n)) . list_sliding_entry $ l

number_divisors 1 = 1
number_divisors n = combinations 1 1 $ primeFactors n
    where
        combinations :: Integral a => a -> a -> [a] -> a
        combinations _ f [] = f
        combinations n f (x:xs)
            | x == n = combinations n (f+1) xs
            | otherwise = f * (combinations x 2 xs)

divisors 1 = [1]
divisors 2 = [1,2]
divisors n = [1] ++ (_divisors n 2 (isqrt n)) ++ [n]
    where
        _divisors n d limit
            | d > limit = []
            | otherwise = if (divisible)
                          then [d] ++ next ++ if (d /= d2)
                                              then [d2]
                                              else []
                          else next
                        where d2 = n `div` d
                              divisible = n `rem` d == 0
                              next = _divisors n (d+1) limit

sum_proper_divisors n = sum . init . divisors $ n

string_to_number_matrix :: String -> [[Integer]]
string_to_number_matrix s = (map (map (\s -> read s :: Integer))) . (map words) . lines $ s

factorial :: Integral a => a -> a
factorial 0 = 1
factorial n = factorial_ 1 n
    where
        factorial_ acc 0 = acc
        factorial_ acc n = factorial_ (acc*n) (n-1)

ilogk :: (Integral a, Integral b) => a -> a -> b
ilogk k n
    | k < 2 = 0
    | n < 2 = 0
    | otherwise = _ilogk 0 k n
    where
        _ilogk :: (Integral a, Integral b) => b -> a -> a -> b
        _ilogk s k n
            | n < k = s
            | otherwise = _ilogk (s+1) k (n `div` k)

is_panDigitalI k n = (is_panDigitalL k) . toDigits $ n
is_panDigitalL k l = (num_length == length l) && (num_length == ((length . nub) $ number)) && ((maximum number) == k)
    where
        number = filter (\d -> 0 < d && d < (k+1)) l
        num_length = fromIntegral k :: Int

create_tuple_orig_and_transf f n = (f n, n)
compare_tuple_pair t = (fst t) == (snd t)

convert_to_binary :: Integer -> [Integer]
convert_to_binary n = _bin n (2^(ilogk 2 n))
    where
        _bin _ 0 = []
        _bin n b
            | n >= b = [1] ++ _bin (n-b) (b `div` 2)
            | otherwise  = [0] ++ _bin n (b `div` 2)

remove_factor 1 n = n
remove_factor m n
    | divisibleby n m = remove_factor m (n `div` m)
    | otherwise = n

-- subsets of size k, including duplicates
combinationsOf 0 _ = [[]]
combinationsOf _ [] = []
combinationsOf k (x:xs) = map (x:) 
  (combinationsOf (k-1) (x:xs)) ++ combinationsOf k xs

--palindromes = [a | a <- [1..], check_palindrome a]
palindromes = n 1
    where   n len = (palindrome_of_length len) ++ (n (len + 1))
            palindrome_of_length len
                | odd len = odd_palindromes (half+1)
                | even len = even_palindromes half
                where half = len `div` 2
                      odd_palindromes = (_palindromes ((:[]) . last) init)
                      even_palindromes = (_palindromes emptyList id)
                        where emptyList _ = []
                      _palindromes f g length_half = (map (fromDigits . (\l -> create_palindrome (f l) (g l)) . toDigits)) $ n
                        where n = [10^(length_half-1)..(10^length_half)-1]
                              --create_palindrome :: (Integral a) => [a] -> [a] -> [a]
                              create_palindrome middle half = half ++ middle ++ (reverse half)
