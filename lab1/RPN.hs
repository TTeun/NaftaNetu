-- rpnEval :: String -> Integer
-- rpnEval

takeDigit :: String -> [String]
takeDigit (c:cs)
            | ((c == '-') || (c == '+') || (c == '*') || (c =='/')) = popFromStack [c]
            | otherwise                                             = pushUntoStack ((takeWhile (/= ' ') (c:cs)) (takeDigit cs))

pushUntoStack :: String -> String -> [String]
pushUntoStack s1 s2 = [s1] ++ [s2]

-- calcFromStack :: Char -> Integer
-- calcFromStack c =

popFromStack :: [Char] -> [String]
popFromStack c = [""]

-- main =  print . rpnEval =<< getLine
