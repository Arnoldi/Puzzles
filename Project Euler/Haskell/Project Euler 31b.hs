import Int_function_lib ()

combinations amount = _comb amount (drop 0 coins)
    where
        coins = [200,100,50,20,10,5,2,1]
        _comb _amount _coins
            | _coins == [] = 0
            | _amount <= 0  = 0
            | remain == 0 = 1 + smaller_coin
            | otherwise = (_comb remain _coins) + smaller_coin
            where
                remain = _amount - (head _coins)
                smaller_coin = _comb _amount (tail _coins)

test = combinations 200
test2 = (map (\i -> combinations i)) $ [0..100]

main = putStrLn . show $ test