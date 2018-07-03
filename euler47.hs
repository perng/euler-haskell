primes' (h:t) = h: (primes' $ filter (\x -> (mod x h) >0) t )
primes = primes' (2:[3,5..])

primePowers = map (\x -> [x^i| i<-[1..]]) primes 


takeWith f (h:_)| not $ f h = []
takeWith f (h:t)| otherwise =  h: (takeWith f t) 


primeFactors x = filter (\d -> (mod x d) ==0) ( takeWith (<= x) primes) 


nfactor:: Int -> Int
nfactor x = length $ primeFactors x 

nf=[(i,n) | i<-[1..], let n= nfactor i]

samenprime:: [(Int,Int)] -> Int
samenprime ((i1,4):(i2,4):(i3,4):(i4,4):t) = i1
samenprime (_:t) = samenprime t

main = print $  samenprime nf

