import Data.List

replaceInRow :: Char -> Int ->  String -> String
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

dirFillRow :: String -> String
dirFillRow xs = fillEm xs (findDbl xs ++ findHls xs)
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

isCorrect :: [String] -> Bool
isCorrect xs
    | not (isDone xs)                = False
    | hasDoubles (transpose xs)      = False
    | hasDoubles xs                  = False
    | not (noTriple xs)              = False
    | not (noTriple (transpose xs))  = False
    | otherwise                      = True

isCorrectTakuzu :: [String] -> Bool
isCorrectTakuzu xs
    | isCorrect (directFill xs) = True
    | otherwise                 = False

takuzu =    ["1.1.1.00",
             "..1...0.",
             ".0...1..",
             "00....11",
             "0..1...1",
             ".1......",
             "...1..0.",
             "11..00.0"]
