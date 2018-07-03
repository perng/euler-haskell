import Data.List

notdivids  y x = mod x y > 0
lessthan y = \x -> x<y
combine x [] = x
combine [] y = y
combine (h1:t1) (h2:t2) | h1 == h2 = h1:combine t1 t2
			| h1 > h2 = h1: combine t1 (h2:t2)
			| otherwise= h2: combine (h1:t1) t2

factorize':: Int -> [Int] -> [Int] -> [Int]
factorize' 1 _ acc = acc
factorize' x [] acc = x:acc
factorize' x (h:t) acc |mod x h ==0 = 
	let nx = div x h 
	    tt = h:(filter (\z -> ((mod z h) >0)) t)
            ttt = filter (\x-> x< nx) tt in 
	factorize' nx ttt (h:acc)
factorize' x (h:t) acc |otherwise   = 
	factorize' x pt acc 
	where pt = filter (\z -> ((mod z h) >0)) t 

factorize x = factorize' x [2..x] []

--main = print $  map factorize [2..10]
--main = print $   foldl1 combine $ map factorize [2..10]
main1 = print $  foldl1 (\x y -> x*y) $ foldl1 combine $ map factorize [2..20]


--gcd x y| x < y = gcd y x 
--		| (mod x y)== 0 = y 
--		| otherwise = gcd y (mod x y) 

--lcm x y = x*y / ( gcd x y) 

main = print $ foldl1 lcm [2..20]




