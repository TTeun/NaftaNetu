-- Computes the sum the squares of its digits
sumOfSquares :: Integer -> Integer
sumOfSquares 0 = 0
sumOfSquares n = (\x -> x*x) (n `mod` 10) + sumOfSquares(n `div` 10)

-- Extends the list of the sum of the squares of the digits by 1
extendList :: [Integer] -> [Integer]
extendList ns = (sumOfSquares $ head ns):ns

-- Computes a small list with happy number. For all number above the lowerbound, the sequence is strictly decreasing.
-- The lower bound = 243 = 3*(9^2)
isSmallList :: [Integer]
isSmallList = [n | n <- [1..243], isHappyNumber n]
    where isHappyNumber :: Integer -> Bool
          isHappyNumber n = isHappyList [n]
                where isHappyList ns
                          | head (extendList ns) == 1      = True
                          | head (extendList ns) `elem` ns = False
                          | otherwise                      = isHappyList(extendList ns)

-- Checks whether a number is happy (using the small list of happy numbers)
isHappy :: Integer -> Bool
isHappy n
    | n < 243 = n `elem` isSmallList
    | otherwise = isHappy(sumOfSquares n)

-- Counts the happy numbers between a certain interval
countHappyNumbers :: Integer -> Integer -> Int
countHappyNumbers a b = length [n | n <- [a .. b], isHappy n]

wrapper :: [String] -> Int
wrapper (a:b:_) = countHappyNumbers (read a::Integer) (read b::Integer)

main =  print . wrapper . words =<< getLine
