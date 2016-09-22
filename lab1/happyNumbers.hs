sumOfSquares :: Int -> Int
sumOfSquares 0 = 0
sumOfSquares n = (\x -> x*x) (n `mod` 10) + sumOfSquares(n `div` 10)

listOfSumOfSquares :: [Int] -> [Int]
listOfSumOfSquares ns = ns ++ [sumOfSquares (last ns)]


hasDouble :: [Int] -> Bool
hasDouble (x:xs)
        | xs == []  = False
        | elem x xs = True
        | otherwise = hasDouble xs

isSmallList :: [Int]
isSmallList = [n | n <- [1..243], isHappyNumber n]

isHappyNumber :: Int -> Bool
isHappyNumber n = isHappyList [n]
    where isHappyList ns
              | last (listOfSumOfSquares ns) == 1  = True
              | hasDouble (listOfSumOfSquares ns)  = False
              | otherwise                          = isHappyList(listOfSumOfSquares ns)

isHappyFast :: Int -> Bool
isHappyFast n
    | n < 243 = n `elem` isSmallList
    | otherwise = isHappyFast(sumOfSquares n)

countHappyNumbers :: Int -> Int -> Int
countHappyNumbers a b = length [n | n <- [a .. b], isHappyFast n]
