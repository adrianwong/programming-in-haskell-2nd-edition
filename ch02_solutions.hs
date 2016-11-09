-- Q5
init' [] = []
init' xs = take (length xs - 1) xs

init'' [] = []
init'' xs = reverse (tail (reverse xs))

init''' []     = []
init''' [_]    = []
init''' (x:xs) = x : init xs

dropLast 0 xs     = []
dropLast 1 (x:xs) = []
dropLast n (x:xs) = x : dropLast (n-1) xs
init'''' xs = dropLast (length xs) xs
