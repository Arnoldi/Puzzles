import Int_function_lib




miller_rabin_prime n -- = (n,compute [2,3],d,j,_list)
    | n < 5 = n==2 || n==3
    | even n = False
    | n < 2047 = compute [2]
    | n < 1373653 = compute [2,3]
    | n < 9080191 = compute [31,37]
    | n < 4759123141 = compute [2,7,61]
    | n < 2152302898747 = compute [2,3,5,7,11]
   -- | otherwise = compute [2..(min (n-1) (2 * (((\x -> x*x) . floor . log . fromIntegral) $ n)))]
    where find_dj d j
             | r == 1 = (d,j) -- if d odd, stop
             | otherwise = find_dj q (j + 1) -- otherwise, divide d by 2 and add 1 to j
             where (q,r) = quotRem d 2
          (d,j) = find_dj (n-1) 0 -- find d,j such that n-1=d*2^j
          compute list_a = and $ zipWith (||) (kongr_test_1 list_a) (kongr_test_2 list_a)

          int_res1 = map ((`mod`n).(^d))
          kongr_test_1 = (map (==1)) . int_res1
          sqrMod m = (m*m) `mod` n
          kongr_test_2 = (map (\i -> or $ (map (==n-1)
                                               (take j $ iterate sqrMod i))))
                         . int_res1



n = 100000
id2 (_,t,_,_,_) = t
prime1 = map isPrime [100000..100000+n]
prime2 = map miller_rabin_prime [100000..100000+n]
speed1 = sum . (filter isPrime) $ [100000..100000+n]
speed2 = sum . (filter miller_rabin_prime) $ [100000..100000+n]
test = zip (zipWith (==) prime1 prime2) [1..]
test3 = filter ((==False) . fst) test
test4 = filter (not) (zipWith (==) prime1 prime2)
main = putStrLn . show $ [speed2,speed2]
--test6 n = (min (n-1) (2 * (((\x -> x*x) . floor . log . fromIntegral) $ n)))
