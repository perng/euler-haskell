import Data.Map(fromList,lookup) 
import Data.Maybe(fromJust)
factors' :: Integer -> [Integer]
factors' x = 
	filter (\y-> (mod x y) ==0) [1.. q  ]   
	where q= (floor (sqrt (fromIntegral x)))

factors::Integer -> [Integer]
factors x = 
	let fac= factors' x in 
	fac ++ [q | i<- fac, let q=div x i, q/=i, q/=x] 

d::Integer -> Integer
d x = sum $ factors x 

main = print $ sum [i+j| i<-[1..10000], let j=d i , j> i, i== d j]
