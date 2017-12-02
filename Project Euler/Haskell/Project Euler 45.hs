import Int_function_lib ()
import Data.List ()

triangleNumbers =   [(n*(  n+1))`div`2|n<-[1..]]
pentagonalNumbers = [(n*(3*n-1))`div`2|n<-[1..]]
hexagonalNumbers =  [ n*(2*n-1)       |n<-[1..]]

search_common tL pL hL
    | tN == pN && tN == hN = [tN] ++ search_common tLs pLs hLs
    | tN < hN = search_common tLs pL hL
    | pN < hN = search_common tL pLs hL
    | otherwise = search_common tL pL hLs
    where tN = head tL
          pN = head pL
          hN = head hL
          tLs = tail tL
          pLs = tail pL
          hLs = tail hL

res = (search_common triangleNumbers pentagonalNumbers hexagonalNumbers) !! 2
main = putStrLn . show $ res