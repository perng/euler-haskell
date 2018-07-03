
apply _ x 0 = x 
apply f x n = apply f (f x) (n-1) 

f n = mod (2*n) 10000000000

main = print $ mod ((apply f 1 7830457) * 28433 +1 ) 10000000000

problem97 = (28433*2^7830457+1) `mod` 10000000000
