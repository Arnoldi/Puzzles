import Int_function_lib (factorial, fromDigits)
import Data.List (sort, splitAt)

deleteAt :: Int -> [a] -> [a]
deleteAt n l = glueback . dropfromSnd $ splitAt n l
    where
        glueback :: ([a],[a]) -> [a]
        glueback t = (fst t) ++ (snd t)
        dropfromSnd :: (a, [b]) -> (a, [b])
        dropfromSnd t = (fst t, drop 1 (snd t))

--permutation :: Ord a => Integral -> [a] -> [a]
permutation _ [] = []
permutation 0 l = l
permutation n l = _perm (n `rem` factorial (length_I l)) (sort l)
    where
        length_I :: [a] -> Integer
        length_I l = fromIntegral (length l) :: Integer
        _perm 0 l = l
        _perm n l = [l !! index] ++ (_perm (n-f*m) (deleteAt index l))
            where m = factorial ((length_I l) - 1)
                  f = n `div` m
                  index = fromIntegral f :: Int

res = fromDigits $ permutation 999999 [0..9]

main = putStrLn . show $ res