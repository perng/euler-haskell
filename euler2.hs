fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
fibevensum (h:_) cum limit| h>=limit = cum
fibevensum (h:t) cum limit = fibevensum t (if (even h) then (cum+h) else cum) limit 
main = print  $ fibevensum fibs 0 4000000
