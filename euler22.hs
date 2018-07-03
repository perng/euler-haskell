import Data.List(sort)
import Data.Char (ord)
score word = sum [  ord ch - ord 'A' + 1  | ch<- word] 
main = do 
	l <-  readFile "p22-names.txt" 
	let scores=map score (  sort (read $ "[" ++ l  ++ "]") ) 
	print $ sum $ zipWith (*) [1..(length scores)] scores
