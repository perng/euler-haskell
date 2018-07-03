import Data.List

--den =[ 10^a - 10^b | a<-[1..], b<-[(a-1),(a-2)..0]]
den =[ 10^a - 1 | a<-[1..]]
tden n = length $ filter (== '9') (show  (head [x |  x<- den, x>n, mod x n ==0]))

gt x y = if (snd x > snd y) then x else y

main= print $ foldl1 gt [(i, tden i) | i<- [3,5..1000]]
