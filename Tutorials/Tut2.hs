listFunc :: [Int] -> Int
listFunc (x:y:_) = x+y
listFunc (x:_)   = x
listFunc _       = 0

and2 :: [Bool] -> Bool
and2 (x:xs) = x && (and2 xs)
and2 []     = True

or2 :: [Bool] -> Bool
or2 (x:xs) = x || (or2 xs)
or2 []     = False

elemNum :: Integer -> [Integer] -> Int
elemNum x xs = length [n | n <- xs, x == n ]

elemNumRec :: Integer -> [Integer] -> Int
elemNumRec _ [] = 0
elemNumRec x (y:ys) | x == y    = 1 + elemNumRec x ys
                    | otherwise = elemNumRec x ys

unique :: [Integer] -> [Integer]
unique xs = [n | n <- xs, (elemNum n xs == 1)]

unique2 :: Eq a => [a] -> [a]
unique2 [] = []
unique2 (x:xs) | not (x `elem` xs) = x : unique2 xs
               | otherwise         = unique2 (filter (/=x) xs)

ins :: Ord a => a -> [a] -> [a]
ins x []                 = [x] --or x:[]
ins x (y:ys) | x > y     = x:y:ys
             | otherwise = y : ins x ys

ins2 :: Ord a => a -> [a] -> [a]
ins2 x []                 = [x] --or x:[]
ins2 x (y:ys) | x > y     = x:y:(unique2 ys)
              | otherwise = y : ins2 x (unique2 ys)

iSort :: Ord a => [a] -> [a]
iSort []     = []
iSort (x:xs) = ins2 x (iSort xs)


isSubList :: String -> String -> Bool
isSubList [] _ = True
isSubList _ [] = False
isSubList (x:xs) (y:ys)
                  | x == y = isSubList xs ys
                  | x /= y = isSubList (x:xs) ys

isSubSequence :: String -> String -> Bool
isSubSequence [] _ = True
isSubSequence _ [] = False
isSubSequence

isWhatSub :: String -> String -> IO ()
isWhatSub s1 s2 | isSubSequence s1 s2  = putStrLn "String 1 is a subsequence of string 2"
                | isSubList s1 s2      = putStrLn "String 1 is a sublist of string 2"
                | otherwise            = putStrLn "String 1 is neither a subsequence or a sublist of string 2"
