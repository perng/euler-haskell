import List
palindrom x = 
  let xx = show x 
      n = div (length xx)   2 in
  (take n xx) == (take n $ reverse xx) 

main= print $  take 1 [p|   x<- [999,998..800], y<-[x,(x-1)..800], let p=x*y, palindrom p]
  
