import Int_function_lib (isqrt)

pentagonal n = (n*(3*n-1)) `div` 2

pentagonal_numbers = _pen 1
    where
        _pen n = [pentagonal n] ++ _pen (n+1)

isPentagonal pn =
    let n = (isqrt(24*pn+1) + 1) `div` 6
        _pn = (n*(3*n-1)) `div` 2
    in (pn == _pn)

test_pair pj pk =
    let ps = pk + pj
        pd = pk - pj
    in (isPentagonal pd) && (isPentagonal ps)

test_pd pd pj =
    let pk = pd + pj
        ps = pk + pj
    in (isPentagonal pk) && (isPentagonal ps)

find_smallest_pentagonal_diff _ [] = 0
find_smallest_pentagonal_diff pd (pj:pen_list) =
    if (test_pd pd pj) then pd else (find_smallest_pentagonal_diff pd pen_list)
    
test1 pent_list = map (\ x -> (find_smallest_pentagonal_diff x pent_list)) pent_list
test n = test1 ((takeWhile (<n)) pentagonal_numbers)

test2 = [(pentagonal j, pentagonal k) | j <- [1..1000], k <- [j+1..1000], test_pair (pentagonal j) (pentagonal k)]
test3 = [(pentagonal d, pentagonal j) | d <- [1..500], j <- [1..10000], test_pd (pentagonal d) (pentagonal j)]

find_pentagonal_pair j k
    | (j == 1) = find_pentagonal_pair k (k+1)
    | otherwise = let   pj = (pentagonal j)
                        pk = (pentagonal k)
                  in if test_pair pj pk then pk-pj else if k>2200 then 0 else find_pentagonal_pair (j-1) k

find_pentagonal_pair2 d j
    | (j == 1) = find_pentagonal_pair2 (d+1) d
    | otherwise = let   pd = (pentagonal d)
                        pj = (pentagonal j)
                  in if test_pd pd pj then pd else if d>2000 then 0 else find_pentagonal_pair2 d (j-1)

pentagonal_search n maxn d j k s pd pj pk ps
 --   | pd == (pk - pj) && ps == (pk + pj) = pd
    | n >= maxn = [0,n,d,j,k,s,pd,pj,pk,ps]
 --   | j >= k = pentagonal_search (n+1) maxn d j (k+1) s pd pj (pentagonal (k+1)) ps
    | pj < (ps - pk) 
    | ps < (pk + pj) = pentagonal_search (n+1) maxn d j k (s+1) pd pj pk (pentagonal (s+1))
    | ps > (pk + pj) = pentagonal_search (n+1) maxn d j (k+1) s pd pj (pentagonal (k+1)) ps
    | pd < (pk - pj) = pentagonal_search (n+1) maxn (d+1) j k s (pentagonal (d+1)) pj pk ps
    
    | pd > (pk - pj) = pentagonal_search (n+1) maxn d (j+1) k s pd (pentagonal (j+1)) pk ps
    
    | otherwise = [1,n,d,j,k,s,pd,pj,pk,ps]

test4 n = pentagonal_search 0 n 1 2 3 4 1 5 12 22