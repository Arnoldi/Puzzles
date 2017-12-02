import Int_function_lib (check_palindrome, toDigits, fromDigits)


lychrel_iterate n = n + (fromDigits . reverse . toDigits $ n)

isLychrel n = _isLychrel n 51
    where
        _isLychrel n 0 = True
        _isLychrel n count = let next = lychrel_iterate n 
                             in if (check_palindrome next)
                                then False
                                else _isLychrel next (count-1)

res = length . (takeWhile (<10000)) . (filter isLychrel) $ [1..]

main = putStrLn . show $ res