import Data.Maybe(fromJust)
singles= [(1,"one"),(2,"two"), (3,"three"),(4,"four"), (5,"five"), (6,"six"), (7,"seven"), (8,"eight"), (9, "nine")]
tens= [(2,"twenty"), (3,"thirty"),(4,"forty"), (5,"fifty"),(6,"sixty"),(7,"seventy"),(8,"eighty"),(9,"ninety")]

teens = [(10,"ten"), (11,"eleven"),(12,"twelve"), (13, "thirteen"), (14, "fourteen"), (15, "fifteen"), (16, "sixteen"), (17, "seventeen"), (18, "eighteen"), (19,"nineteen")]

hundred="hundred"
thousand="thousand" 


translate:: Int -> [Char]
translate n = 
	let 	thousands = div n 1000
		hundred_remain = mod n 1000 		
		hundreds = div (hundred_remain - (mod hundred_remain 10) ) 100
		tens_remain = mod n 100
		ttens = div (tens_remain - (mod n 10) ) 10
		ones = mod n 10 in 
		--(thousands, hundreds, ttens, ones)
		(if (thousands ==0) then "" else  (fromJust $ lookup thousands singles) ++ " " ++ thousand ) ++
		(if (hundreds ==0) then "" else  (fromJust $ lookup hundreds singles) ++ " " ++ hundred ) ++
		(if (n>100 && (tens_remain >0)) then " and " else "") ++
		(if (tens_remain>9 && tens_remain<20) then (fromJust $ lookup tens_remain teens) 
		 else 
		    ((if (ttens ==0) then "" else  (fromJust $ lookup ttens tens) ++ " " ) ++
		    (if (ones ==0) then "" else  (fromJust $ lookup ones singles)  ) ))

mylen s = length $ filter (/= ' ') s
--main = print   [ translate n | n<- [1..1000]]
main = print $ sum $ map mylen [ translate n | n<- [1..1000]]
	

