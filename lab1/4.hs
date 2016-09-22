expmod :: Integer -> Integer -> Integer -> Integer
expmod b 0 m = 1
expmod b e m
    | e `mod` 2 == 0 = expmod (b * b `mod` m) (e `div` 2) m `mod` m
    | otherwise      = b * expmod (b * b `mod` m) (e `div` 2) m `mod` m

findDigits :: Integer -> Integer -> Integer
findDigits n d = sum [expmod n k (10^d) | k <- [0..n]]

int2List :: Integer -> [Integer]
int2List n = makeList n []
    where
        makeList n xs
            | n == 0    = xs
            | otherwise = makeList (n `div` 10) ([n `mod` 10] ++ xs)

lastDigits :: Integer -> Integer -> [Integer]
lastDigits n = int2List . findDigits n 
