notdivids  y x = mod x y > 0
largestPrimeFactor x [h]= h
largestPrimeFactor x (h:t)= 
	if  mod x h ==0 then  
		let x'= div x h in 
		largestPrimeFactor x' (filter (notdivids h) [(h+1)..x'])
	else 
		largestPrimeFactor x (filter (notdivids h) [(h+1)..x])

main = print $ largestPrimeFactor  600851475143 [3,5..600851475143 ]



