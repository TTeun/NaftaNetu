fibo :: [Integer]
fibo = [1,1] ++ makeIt [2,3,5]
    where
        makeIt (x1:x2:xs) = makeIt ((x1+x2) : xs)