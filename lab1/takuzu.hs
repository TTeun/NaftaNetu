import Data.List

replaceInRow :: Char -> Int -> String -> String
replaceInRow c n xs = take (n - 1) xs ++ [c] ++ drop (n) xs

findDbl :: String -> [(Char, Int)]
findDbl xs = [('1', n) | n <- [1..u], take 2 (drop n xs) == "00", head (drop (n - 1) xs) == '.']
          ++ [('0', n) | n <- [1..u], take 2 (drop n xs) == "11", head (drop (n - 1) xs) == '.']
            where
                u = length xs - 1

findHls :: String -> [(Char, Int)]
findHls xs = [('1', n + 2) | n <- [0..u], take 3 (drop n xs) == "0.0"]
          ++ [('0', n + 2) | n <- [0..u], take 3 (drop n xs) == "1.1"]
            where
                u = length xs - 3

findLast :: String -> [(Char, Int)]
findLast xs
    | length (filter (== '0') xs) == (length xs) `div` 2 = [('1', n + 1) | n <- [0..l], xs !! n == '.']
    | length (filter (== '1') xs) == (length xs) `div` 2 = [('0', n + 1) | n <- [0..l], xs !! n == '.']
    | otherwise = []
        where
            l = (length xs - 1)

dirFillRow :: String -> String
dirFillRow xs = fillEm xs (findDbl xs ++ findHls xs ++ findLast xs)
    where
        fillEm xs []     = xs
        fillEm xs (d:ds) = fillEm (replaceInRow (fst d) (snd d) xs) ds

manyFill :: String -> String
manyFill = dirFillRow . reverse . dirFillRow . reverse

fillRow :: String -> String
fillRow xs = fillEm xs (manyFill xs)
    where
        fillEm xs ys
            | xs == ys  = xs
            | otherwise = fillRow ys

fillAllRows :: [String] -> [String]
fillAllRows xs = [fillRow x | x <- xs]

fillAllColumns :: [String] -> [String]
fillAllColumns = transpose . fillAllRows . transpose

fillAll :: [String] -> [String]
fillAll = fillAllColumns . fillAllRows

directFill :: [String] -> [String]
directFill xs = fillIt xs (fillAll xs)
    where
        fillIt xs ys
            | xs == ys  = xs
            | otherwise = directFill ys


hasDoubles :: Eq a => [a] -> Bool
hasDoubles []     = False
hasDoubles (x:xs) = (x `elem` xs) || hasDoubles xs

noTripleInRow :: String -> Bool
noTripleInRow xs = checkEm [ take 3 (drop n xs) | n <- [0..length xs - 3] ]
    where
        checkEm ls
            | "000" `elem` ls = False
            | "111" `elem` ls = False
            | otherwise       = True

noTriple :: [String] -> Bool
noTriple xss = and [noTripleInRow xs | xs <- xss]

isDone :: [String] -> Bool
isDone = and . map (not . elem '.')

tooMany :: [String] -> Bool
tooMany xss = or [length (filter (== c) xs) > (length xs) `div` 2| c <- ['0', '1'], xs <- xss]

isCorrect :: [String] -> Bool
isCorrect xs
    | tooMany (transpose xs)         = False
    | tooMany xs                     = False
    | not (noTriple xs)              = False
    | not (noTriple (transpose xs))  = False
    | otherwise                      = True

hasDot :: [String] -> Bool
hasDot xss = or ['.' `elem` xs | xs  <- xss]

replaceFirstDot :: [String] -> Char -> [String]
replaceFirstDot xss c = fillIt xss c 0
    where
        fillIt xss c n
            | '.' `elem` xs = (takeWhile (/= '.') xs ++ [c] ++ (tail $ (dropWhile (/= '.')) xs)) : (tail xss)
            | otherwise     = (head xss) : fillIt (tail xss) c (n + 1)
                where
                    xs = head xss

makeAllCombs :: [String] -> [[String]]
makeAllCombs xs = makeEm [directFill xs]
    where
        makeEm xss
            | or [hasDot xs | xs <- xss] = makeEm $ filter (isCorrect) $ ds ++ [replaceFirstDot ns '0' | ns <- nss] ++ [replaceFirstDot ns '1' | ns <- nss]
            | otherwise                  = xss
                where
                    ds = [xs | xs <- xss, not (hasDot xs)]
                    nss = filter (hasDot) xss

solSet :: [String] -> [[String]]
solSet xs = filter (isCorrect) (makeAllCombs xs)

isCorrectTakuzu :: [String] -> Bool
isCorrectTakuzu xs = length [ss | ss <- (solSet xs), not(hasDoubles ss), not (hasDoubles (transpose ss))] == 1

-- takuzu =    ["1.1.1.00",
--              "..1...0.",
--              ".0...1..",
--              "00....11",
--              "0..1...1",
--              ".1......",
--              "...1..0.",
--              "11..00.0"]

-- takuzu = ["110...","1...0.","..0...","11..10","....0.","......"]

main =  print . isCorrectTakuzu .lines =<< getContents
