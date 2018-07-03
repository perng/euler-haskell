main= print $ head [a*b*c| c<-[334..499], b<-[c,(c-1)..(div (1000-c) 2)],let a=1000-b-c, c*c==a*a+b*b]

