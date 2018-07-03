allnum = 2:[3,5..2000000]

primes'  [] = []
primes' (h:t) = h: (primes' $ filter (\x -> (mod x h) >0) t )

primes = primes' allnum

main = print $ sum primes

