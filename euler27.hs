import Data.Tuple
primes' (h:t) = h: (primes' $ filter (\x -> (mod x h) >0) t )
primes = primes' (2:[3,5..])

sortedElem  (h:_) a| a>h = False
sortedElem  (h:_) a| a==h = True
sortedElem  (_:t) a| otherwise = sortedElem  t a

formulas::[(Int,Int, Int -> Int)]
formulas = [(a,b,\n-> n*n+a*n+b) | a <- [2..1000], b<-[2..1000]]

iterations = [[(length $ sortedElem $f i,a,b) |i<-[1..]]| (a,b,f)<- formulas]

compare1 (x,a1,b1) (y,a2,b2) |  x>y =  (x,a1,b1) 
		| otherwise = (y,a2,b2) 

main = print $ foldl1 compare1 iterations
