import Int_function_lib ()
import Data.Array

coins :: [Int]
coins = [200,100,50,20,10,5,2,1]
numCoinTypesLessOne = length coins - 1
coinsArray = listArray(0, numCoinTypesLessOne) $ coins

memoCombinations = listArray(0,numCoinTypesLessOne) . (map memoCombinationsByCoin) $ [0..numCoinTypesLessOne]
    -- create an array for every coin, with a list giving the number of possible combinations
    -- for the amounts using coins of at most the value associated with the place in the array
    where memoCombinationsByCoin = (\c-> map (_comb c) [0..])
          _comb coinInd _amount
            | coinInd == numCoinTypesLessOne = 1
            | _amount == 1 = 1
            | remain  <  0 = usingSmallerCoin
            | remain  == 0 = usingSmallerCoin + 1
            | otherwise    = usingSmallerCoin + combinationsByCoin coinInd remain
            where
                remain = _amount - (coinsArray ! coinInd)
                usingSmallerCoin = if   coinInd < (numCoinTypesLessOne)
                                   then combinationsByCoin (coinInd + 1) _amount
                                   else 0

combinationsByCoin coinIndex amount = (memoCombinations ! coinIndex) !! amount
combinations amount
    | amount < 0  = 0
    | otherwise   = combinationsByCoin 0 amount


res = combinations 200

main = putStrLn . show $ res

--combinations2 amount = _comb amount (drop 0 coins)
--    where
--        coins = [200,100,50,20,10,5,2,1]
--        _comb _amount _coins
--            | _coins == [] = 0
--            | _amount <= 0  = 0
--            | remain == 0 = 1 + smaller_coin
--            | otherwise = (_comb remain _coins) + smaller_coin
--            where
--                remain = _amount - (head _coins)
--                smaller_coin = _comb _amount (tail _coins)