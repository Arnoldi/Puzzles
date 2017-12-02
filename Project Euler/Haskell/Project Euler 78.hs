piles_coins1 n
	| n <= 0 = 0
	| n == 1 = 1
	| n == 2 = 2
	| otherwise = piles_coins1 (n-2) * 2 + 1

piles_coins2 n
	| n <= 0 = 0
	| n == 1 = 1
	| n == 2 = 2
	| otherwise = 1 + sum (map piles_coins2 [(n-k) `div` k | k <- [1..(n `div` 2)]])
	
test1 n = [(n-k) `div` k | k <- [1..(n `div` 2)]]

piles_coins3 n
	| n <= 0 = 0
	| n == 1 = 1
	| n == 2 = 2
	| otherwise = sum (map (\x ->  $ map piles_coins3 [n - k*(3*k-1) `div` 2 | k <- [1..n]])