primes :: [Integer]
primes = sieve [2..]
    where
        sieve :: [Integer] -> [Integer]
        sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

composites :: [(Integer, [Integer])]
composites = getEm [4..] primes
    where
        getEm :: [Integer] -> [Integer] -> [(Integer, [Integer])]
        getEm (x:xs) ps
            | head primeSet == x = getEm xs (tail primeSet)
            | otherwise          = (x,getPrimeFac x) : getEm xs primeSet
                where
                    primeSet = dropWhile (<x) ps

getPrimeFac :: Integer -> [Integer]
getPrimeFac c = reverse $ getFac c []
    where
        getFac 1 ls = ls
        getFac n ls = getFac (n `div` d) (d : ls)
            where 
                d = head $ dropWhile (\x -> n `mod` x /= 0) primes

wrapper :: String -> [(Integer,[Integer])]
wrapper input = take (read input::Int) composites

main =  print . wrapper =<< getLine