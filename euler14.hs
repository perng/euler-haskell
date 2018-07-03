import Data.Map
import Data.List(sort)
import Maybe

simpleIter 1 = 1 
simpleIter n| even n = 1+ (simpleIter (div n 2)) 
simpleIter n| otherwise  = 1+ (simpleIter (3*n+1))

real_main = print $  foldl1 mymax $ zip [1..1000000] (Prelude.map simpleIter [1..1000000])

main = print $  (sort $ zip (Prelude.map simpleIter [1..1000000]) [1..1000000] ) !!500000


iter' 1 m= (1, m)
iter' n m= case (Data.Map.lookup n m ) of 
	Nothing -> 
		let n' = (if (even n) then (div n 2) else (3*n+1))
		    result = (iter' n'   m)
		    mmm = (insert n (1+(fst result)) (snd result)) in 
		(1+(fst result), mmm) 
	Just c -> (c, m) 

iter m n  = iter' n  m

mmap f state [] = []
mmap f state (h:t) = 
	let (result, state') = f state h in 
	result: (mmap f state' t)

mymax (a,b) (c,d)| b>d = (a,b) 
		| otherwise = (c,d) 

main2 = print $ foldl1 mymax $ zip [1..1000000] ( mmap iter empty [1..1000000])


ss 1 = [1 ]
ss n| even n = n': (ss n')  where n'=(div n 2) 
ss n| otherwise  = n': (ss n') where n'= (3*n+1)
