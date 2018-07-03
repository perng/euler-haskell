triangle = map (\x-> div (x*(x+1)) 2) [1..]
pentagonal = map (\x-> div (x*(3*x-1)) 2) [1..]
hexagonal =map (\x-> x*(2*x-1)) [1..]
common (h1:t1) (h2:t2) (h3:t3) | h1< h2 = common t1 (h2:t2) (h3:t3) 
                               | h2< h1 = common (h1:t1) t2 (h3:t3) 
                               | h2< h3 = common (h1:t1) t2 (h3:t3) 
                               | h3< h2 = common (h1:t1) (h2:t2) t3 
                               | otherwise = h1: (common t1 t2 t3)
main1 = print $ head (filter (>40755)  (common triangle pentagonal hexagonal))



common' (h1:t1) (h2:t2)  | h1< h2 = common' t1 (h2:t2) 
                               | h2< h1 = common' (h1:t1) t2 
                               | otherwise = h1: (common' t1 t2 )


main = print $ head (filter (>40755)  (common' pentagonal hexagonal))
