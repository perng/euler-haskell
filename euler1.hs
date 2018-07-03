threeorfive x = (mod x 3 ==0) || (mod x 5 == 0)

main = print $ sum $filter threeorfive [1..999]
