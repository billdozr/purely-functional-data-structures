{-
Exercise 2.1

Solution in O(n)
-}

suffixes [] = [[]]
suffixes xs = xs : (suffixes $ tail xs)
