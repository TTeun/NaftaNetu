min2 :: Int -> Int -> Int
min2 x y = if x < y then x else y

minThree :: Int -> Int -> Int -> Int
minThree x y z = min2 z (min2 x y)


-- isDigit :: Char -> Bool
-- isDigit x = fromEnum x - 48 >= 0 && fromEnum x - 48 <= 9

--charToNum :: Char -> Int
--charToNum x =  if isDigit x then fromEnum x - 48 else 0

-- charToNum :: Char -> Int
-- charToNum x
--           | x `elem` ['0' .. '9'] = fromEnum x - (fromEnum '0')
--           | otherwise = 0

charToNum :: Char -> Int
charToNum x
          | isDigit x = fromEnum x - 48
          | otherwise = 0
              where
                  isDigit :: Char -> Bool
                  isDigit x = fromEnum x - 48 >= 0 && fromEnum x - 48 <= 9

numberDroots :: Float -> Float -> Float -> Int
numberDroots a b c
             | b^2 - 4 * a * c < 0 = 0
             | b^2 - 4 * a * c > 0 = 2
             | otherwise           = 1

numberRoots :: Float -> Float -> Float -> Int
numberRoots a b c
            | a /= 0    = numberDroots a b c
            | b /= 0    = 1
            | c /= 0    = 0
            | otherwise = 3


smallerRoot :: Float -> Float -> Float -> Float
smallerRoot a b c
            | numberRoots a b c == 1 = (-b) / (2 * a)
            | numberRoots a b c == 2 = ((-b) - (sqrt(b^2 - 4 * a * c))) / (2 * a)
            | otherwise              = 0

largerRoot :: Float -> Float -> Float -> Float
largerRoot a b c
            | numberRoots a b c == 1 = (-b) / (2 * a)
            | numberRoots a b c == 2 = ((-b) + (sqrt(b^2 - 4 * a * c))) / (2 * a)
            | otherwise              = 0

rangeProduct :: Int -> Int -> Int
rangeProduct m n
             | n < m     = 0
             | otherwise = product [m .. n]

rangeProduct2 :: Int -> Int -> Int
rangeProduct2 m n
            | n < m     = 0
            | n == m    = m
            | otherwise = n*rangeProduct2 m (n-1)

prop_rangeProduct m n
            | abs m > 100000   = True
            | abs n > 100000   = True
            | otherwise = rangeProduct m n == rangeProduct2 m n

fac :: Int -> Int
fac = rangeProduct 1

pow2 :: Integer -> Integer
pow2 n
      | n == 0 = 1
      | even n = sq (pow2(n `div` 2))
      | otherwise = 2 * pow2(n - 1)
          where sq x = x*x

maxOccurs :: Integer -> Integer -> (Integer,Integer)
maxOccurs x y
            | x == y    = (x, 2)
            | otherwise = (max x y, 1)
                  where max x y = if x > y then x else y

maxThreeOccurs :: Integer -> Integer -> Integer -> (Integer,Integer)
maxThreeOccurs x y z
                    | (x == y) && (x == z) = (x, 3)
                    | x > y                = maxOccurs x z
                    | otherwise            = maxOccurs y z

doubleAll :: [Integer] -> [Integer]
doubleAll xs = [2*n | n <- xs]

matches :: Integer -> [Integer] -> [Integer]
matches n xs = [y | y <- xs, n == y]

isElementOf :: Integer -> [Integer] -> Bool
isElementOf n xs = matches n xs /= []
