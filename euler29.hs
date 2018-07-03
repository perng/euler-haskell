import Data.List 
import Data.Set
main1 =  print $ length $ group $ sort [a^b| a<- [2..100], b<-[2..100]]
lmain =  print $ length $ nub [a^b| a<- [2..100], b<-[2..100]]

main2 = print $ size $ Data.Set.fromList [x^y | x <- [2..100], y <- [2..100]]
