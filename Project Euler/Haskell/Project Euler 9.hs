import Int_function_lib (isqrt, divisibleby, remove_factor)

n = 1000

pythagorean_triplet a b c n
    | a^2 + b^2 > c^2 = pythagorean_triplet a b (c+1) n
    | a+b+c > n = pythagorean_triplet (a-1) b (c-1) n
    | a^2 + b^2 < c^2 || a+b+c < n = pythagorean_triplet a (b+1) c n
    | otherwise = [a,b,c,a*b*c]

res = pythagorean_triplet n 1 0 n

main = putStrLn . show $ res

square x = x*x

s = 1000
s2 = s `div` 2
sqrt_s2 = isqrt s2
mlimit = sqrt_s2 + if square sqrt_s2 == s2
                   then -1
                   else 0

m = 2
test =  if divisibleby m s2
        then
            let sm = remove_factor 2 $ s2 `div` m
                k = m + if odd m
                        then 2
                        else 1
            in k
        else 0