toSqDigitSum :: Integer -> Integer
toSqDigitSum n = computeSum n 0
    where
        computeSum 0 s = s
        computeSum n s = computeSum q $ s + (\x -> x * x) r
            where
                (q,r) = quotRem n 10

doubles :: Eq a => [a] -> Bool
doubles []     = False
doubles (x:xs) = (x `elem` xs) || doubles xs

isHappyNaive :: Integer -> Bool
isHappyNaive n = checkIt [n]
    where
        checkIt xs
            | head xs == 1  = True
            | doubles xs    = False
            | otherwise     = checkIt $ toSqDigitSum (head xs) : xs

smallList :: [Integer]
smallList = [n | n <- [1..243], isHappyNaive n]

isHappy :: Integer -> Bool
isHappy n
    | n < 243   = n `elem` smallList
    | otherwise = isHappy $ toSqDigitSum n

countHapppy :: Integer -> Integer -> Int
countHapppy a b = length [n | n <- [a..b], isHappy n]
