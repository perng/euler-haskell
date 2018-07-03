fib _ n count | n> 10^999 = count 
fib a b count = fib b (a+b) (count+1) 

main = print $ fib 1 1 2
