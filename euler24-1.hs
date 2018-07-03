bases = [product [1..n] | n<- [10,9..1]]

trans n [] = [] 
trans n (h:t) = (div n h): (trans (mod n h) t) 

main = print $ trans 1000000 bases
--main = print $ trans 1000000 bases
