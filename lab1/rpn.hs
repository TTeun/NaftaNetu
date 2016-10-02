--1. takeWhile until you meet een operator (gives a string of numbers and spaces) --> "13 6 12 * 6" = "13 6 12 "
--2. Convert string numbers to integers; numbers are separated by spaces --> "13 6 12 " = 13 -> 6 -> 12
--2. apply operator to the integers
--3. repeat 1-3 until list is empty


rpnEval :: String -> String
rpnEval [] = ?
rpnEval ss = applyOp (takeWhile (isDigitSpace) ss) (head (tail ss)) -- (head (tail ss)) moet dus de operator zijn; het volgende element na de takeWhile
    where isDigitSpace :: Char -> Bool
          isDigitSpace x = ((fromEnum x - 48 >= 0 && fromEnum x - 48 <= 9) || fromEnum x == 32)

stringNumToInt :: String -> Integer
stringNumToInt ss = read (takeWhile (isSpace) ss) :: Integer
    where isSpace :: Char -> Bool
          isSpace x = fromEnum x == 32

applyOp :: String -> Char -> Integer
applyOp ?? = stringNumToInt ss
applyOp ss op
    | (op == '+') = stringNumToInt ss + applyOp (stringNumToInt ss) op)) -- dit moet dus de recursieve worden. Je kan alleen de lijst niet splitsen ivm meervoudige getallen; "12" zou dan 1 worden
    | (op == '-') = stringNumToInt ss - applyOp (stringNumToInt ss) op))
    | (op == '*') = stringNumToInt ss * applyOp (stringNumToInt ss) op))
    | (op == '/') = stringNumToInt ss / applyOp (stringNumToInt ss) op))
