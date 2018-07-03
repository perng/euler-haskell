import Data.List
exp5 = [(n,e)| n<- [9,8..0], let e= n^5]

maxdigit = head [n | n<-[1..], 10^n-1 > n* (9^5)]

match 0 0 = true  
match 0 _ = false  
match digits balance  
