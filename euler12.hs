nat = [1..]

trinumber' acc (h:t) = (acc+h) : (trinumber' (acc+h) t)  
trinumbers = trinumber' 0 nat 

test x = [1..(floor $ sqrt x)]

factors' :: Integer -> [Integer]
factors' x = filter (\y-> (mod x y) ==0) [1.. q  ]   where q=floor $ sqrt x
--nfactor:: Int -> Int
--nfactor x = (length . factors' x)*2 

--main = print $ take 1 [n| n<- trinumbers, (nfactor n)>=20]
