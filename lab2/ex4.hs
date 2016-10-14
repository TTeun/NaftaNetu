toBeRemoved :: Integer -> [Integer]
toBeRemoved n = filter (<= n) [i + j + 2 * i * j | i <- [1..iLimit], j <- [1..jLimit i]]
    where 
        nFlt     = fromIntegral n 
        iFlt i   = fromIntegral i
        iLimit   = floor (sqrt (nFlt / 2))
        jLimit i = floor ((nFlt - (iFlt i)) / (2 * (iFlt i) + 1))

primes :: [Integer]
primes = 2 : map (\n -> 2 * n + 1) ([n | n <- [1..], not (n `elem` (toBeRemoved n))])

wrapper :: String -> [Integer]
wrapper input = take (read input::Int) primes

main =  print . wrapper =<< getLine