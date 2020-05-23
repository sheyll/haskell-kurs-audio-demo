test = q [3, 2, 1]


q []       = []
q (x : xs) = q (filter (< x) xs) ++ [x] ++ q (filter (>= x) xs)
