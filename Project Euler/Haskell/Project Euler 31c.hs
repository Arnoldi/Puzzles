import Int_function_lib ()
import Data.Array

coins :: [Int]
coins = [200,100,50,20,10,5,2,1]

problem_31 = (ways coins !!)
ways [] = 1 : repeat 0
ways (coin:coins) =n 
    where n = zipWith (+) (ways coins) (replicate coin 0 ++ n)

res = problem_31 200

main = putStrLn . show $ res