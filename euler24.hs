import List 
bases = [product [1..n] | n<- [9,8..0]]

trans n [1] = [n] 
trans n (h:t) = (div n h): (trans (mod n h) t) 

gen _ [] = []
gen l (h:t) = l!!h : (gen (delete (l!!h) l) t)

main = print $ gen "0123456789" ( trans 1 bases)
