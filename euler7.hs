allint = 2:[3,5..]

primes' (h:t) = h: (primes' $ filter (\x -> (mod x h) >0) t )

primes = primes' allint

main1 = print $ last $ take 10001 primes 


---- solution by other people
isMultipleOf x y = ( x `mod` y ) == 0

isPrime 2 = True
isPrime n = not.or $ fmap (n `isMultipleOf`) [ 2 .. (ceiling.sqrt.fromIntegral) n ]

primess = filter isPrime (2:[3,5..])

problem7 = last $ take 10001 primess

main = print $ problem7



--- sieve is realy slow, don't know why
