isMultipleOf x y = ( x `mod` y ) == 0

isPrime 2 = True
isPrime n = not.or $ fmap (n `isMultipleOf`) [ 2 .. (ceiling.sqrt.fromIntegral) n ]

primess =  filter isPrime (2:[3,5..2000000])

main = print $ sum primess


