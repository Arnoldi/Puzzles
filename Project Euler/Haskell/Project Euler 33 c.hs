--import Int_function_lib (toDigits, fromDigits, create_tuple_orig_and_transf, compare_tuple_pair)
import Data.Ratio

candidates = [a%c | a<-[1..9], b<-[1..9], c<-[1..9], a /= c, isCurious a b c]
isCurious a b c = (10*a+b)%(10*b+c) == (a%c)
res = denominator . product $ candidates
main = putStrLn . show $ res
