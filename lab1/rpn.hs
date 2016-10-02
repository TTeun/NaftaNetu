--1. Take elements until you meet an operator (gives a string of numbers and spaces) --> "13 6 12 + *" = "13 6 12 "
--2. Convert string from step 1 to a list of integers; --> "13 6 12 " = [13,6,12]
--3. apply operator to the first two elements of the list of integers.
--   Replace the list from step 1 by the outcome -> outcome: 6+12=20 -> "20 6 *"
--4. repeat 1-3 until the string only has digits (and spaces) in them

rpnEval :: String -> Integer
rpnEval ss
    | and (map (isDigitSpace) ss) == True = read ss :: Integer
    | otherwise                           = rpnEval ((processString tt (head dd)) ++ (dropWhile (isSpace)(drop 1 dd)))
        where
              (tt, dd) = span (isDigitSpace) (deSpace ss)

--Loses extra spaces
deSpace :: String -> String
deSpace ss = loseEm (dropWhile (isSpace) ss)
    where
        loseEm "" = ""
        loseEm (s:ss)
            | isSpace s = ' ' : (loseEm (dropWhile (isSpace) ss))
            | otherwise = s : loseEm ss

--Applies the operator to the last two elements of the list
applyOp :: [Integer] -> Char -> [Integer]
applyOp xs op = reverseIt (reverse xs) op
    where
        reverseIt (x1:x2:xs) op
          | op == '+' = reverse ((x1 + x2) : xs)
          | op == '-' = reverse ((x2 - x1) : xs)
          | op == '*' = reverse ((x1 * x2) : xs)
          | op == '/' = reverse ((x2 `div` x1) : xs)


toString :: [Integer] -> String
toString [] = ""
toString (x:xs) = (show x) ++ " " ++ (toString xs)

--Processes the string in order to get the right elements and give back the correct outcome
processString :: String -> Char -> String
processString ss op = toString (applyOp (getNumbers ss) op)

--Converts all the numbers in the given string to a list of integers
getNumbers :: String -> [Integer]
getNumbers "" = []
getNumbers " " = []
getNumbers ss = (stringNumToInt (takeWhile (not . isSpace) rem)) : getNumbers (dropWhile (not . isSpace) rem)
    where
        rem = (dropWhile (isSpace) ss)
        stringNumToInt :: String -> Integer
        stringNumToInt ss = read (dropWhile (isSpace) ss) :: Integer

--Checks whether or not it is a space
isSpace :: Char -> Bool
isSpace x = fromEnum x == 32

--Checks whether or not it is a digit or a space
isDigitSpace :: Char -> Bool
isDigitSpace x = ((fromEnum x - 48 >= 0 && fromEnum x - 48 <= 9) || fromEnum x == 32)

main =  print . rpnEval =<< getLine
