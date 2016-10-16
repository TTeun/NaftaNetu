type Name = String
type Domain = [Integer]

data Expr = Val Integer
    | Var Name
    | Expr :+: Expr
    | Expr :-: Expr
    | Expr :*: Expr
    | Expr :/: Expr
    | Expr :%: Expr
    deriving Eq

--In order to print it pretty (with brackets) the function show is implemented differently
showPretty :: String -> String
showPretty s = "(" ++ s ++ ")"

--An instance is needed for the function Show combined with Expr. There are only 3 types that need to be shown
--1. a variable (Var). This is already a string so it just needs to print the input
--2. an integer (Val). This needs to be converted to a string which can be done with "show 'input'"
--3. an operator. Before and after the operator there can be a Var or a Val.
--   So it needs to call the Showpretty twice and concatenated in between with a string that 'prints' the operator
instance Show Expr where
    show (Var x)   = x
    show (Val y)   = show y
    show (p :+: q) = showPretty(show p ++ "+" ++ show q)
    show (p :-: q) = showPretty(show p ++ "-" ++ show q)
    show (p :*: q) = showPretty(show p ++ "*" ++ show q)
    show (p :/: q) = showPretty(show p ++ "/" ++ show q)
    show (p :%: q) = showPretty(show p ++ "%" ++ show q)

--Below decription for 'vars' and 'uniq'
--In order to get the sorted list of variables we need to add a variable to a list first.
--Then we add the next variable at the beginning of the list.
--We then remove that variable from the rest of the list (using filter) since we want the list to be unique
--After that we sort the completed variable list using selection sort
vars :: Expr -> [Name]
vars (Var x)   = [x]
vars (Val y)   = []
vars (p :+: q) = uniq(vars p ++ vars q)
vars (p :-: q) = uniq(vars p ++ vars q)
vars (p :*: q) = uniq(vars p ++ vars q)
vars (p :/: q) = uniq(vars p ++ vars q)
vars (p :%: q) = uniq(vars p ++ vars q)

uniq :: Ord a => [a] -> [a]
uniq []     = []
uniq (x:xs) = sortList(x:uniq(filter (/= x) xs))
    where
          sortList :: Ord a => [a] -> [a]
          sortList []          = []
          sortList (x:[])      = [x]
          sortList (x1:x2:xs)
              | x1 < x2        = x1 : sortList (x2:xs)
              | otherwise      = x2 : sortList (x1:xs)

type Valuation = [(Name, Integer)]

-- for the variables it takes the integer (if it exists)
-- for the values it just takes the integer
-- for an operation it puts a found integer (from value or variable) before the operator and another found integer after and applies the operator (recursively)
evalExpr :: Expr -> Valuation -> Integer
evalExpr (Var x) v   = verify (lookup x v)
evalExpr (Val y) v   = y
evalExpr (p :+: q) v = evalExpr p v + (evalExpr q v)
evalExpr (p :-: q) v = evalExpr p v - (evalExpr q v)
evalExpr (p :*: q) v = evalExpr p v * (evalExpr q v)
evalExpr (p :/: q) v = evalExpr p v `div` (evalExpr q v)
evalExpr (p :%: q) v = evalExpr p v `mod` (evalExpr q v)

--This checks whether or not an integer exists for the given variable name. An error occurs if there is no such integer. Otherwise it return the integer
verify :: Maybe Integer -> Integer
verify b
        | b == Nothing = error "Valuation is not complete"
        | otherwise    = sure b
            where sure (Just b) = b

--For every variable and domain it makes a seperate valuations list for every value in the domain (using toVal)
--Then it adds every element in 2 seperate lists to eachother constructing a list of valuations for 2 variables
--Then another single list is added to the combined list from above using map.
--In the end, after adding all the list together, it folds it correctly
valuations :: [(Name,Domain)] -> [Valuation]
valuations ds = foldl (addVals) [] $ map toVal ds
    where
          toVal :: (Name,Domain) -> [Valuation]
          toVal ([], _)      = []
          toVal (cs, xs)     = [[(cs, x)] | x <- xs]

          addVals :: [Valuation] -> [Valuation] -> [Valuation]
          addVals [] ys   = ys
          addVals xs []   = xs
          addVals xss yss = [ xs ++ ys | xs <- xss, ys <- yss]

-- e equals the expression of Pythagorean triplet (evaluated to zero): a^2+b^2-c^2=0
-- vs is the valuations list for a given domain. Starting for a at 3, b at 4 and c at 5 since this the first Pythagorean triplet.
--    The endings are constructed given the constraints that a<=b<=c.
--    Also equals like [("a", 5), ("b", 5), ("c", 5)] need to be filtered out using these conditions
--    Also duplicates like (3,4,5) and (4,3,5) need to be filtered out using these conditions
--Then the correct valuations list for the triplets is constructed using list comprehesion for when in 'vs' the 'e' equals zero
pytriples :: Integer -> [Valuation]
pytriples n = doIt e vs
    where
         e = ((Var "a" :*: Var "a") :+: (Var "b" :*: Var "b") :-: (Var "c" :*: Var "c"))
         vs = filter (conditions) (valuations [("a", [3..(n-2)]), ("b", [4..(n-1)]), ("c", [5..n])])
         conditions :: Valuation -> Bool
         conditions v = (a < b) && (b < c)
             where
                 a = verify (lookup "a" v)
                 b = verify (lookup "b" v)
                 c = verify (lookup "c" v)
         doIt :: Expr -> [Valuation] -> [Valuation]
         doIt e vs = [v | v <- vs, 0 == evalExpr e v]

--In order to get all the elements (Var, Val and operator) seperately and ignore the spaces functions like isDigit, isSpace and isAlpha are needed
--It then scans for every char of the string what it is. Since a number or letter can be longer than just 1 character, the functions takeWhile and dropWhile need to be used
--So the elements (Val, Var, operator) are seperated and then concatenated into a list of strings where every string is a seperate element (Val, Var, operator)
tokenize :: String -> [String]
tokenize [] = []
tokenize (c : cs)
                | isDigit c == True = [takeWhile (isDigit) (c:cs)] ++ (tokenize (dropWhile (isDigit) cs))
                | isSpace c == True = (tokenize cs)
                | isAlfa c == True  = [takeWhile (isAlfa) (c:cs)] ++ (tokenize (dropWhile (isAlfa) cs))
                | otherwise           = [[c]] ++ (tokenize cs)
                    where
                         isSpace :: Char -> Bool
                         isSpace c = fromEnum c == 32

isAlfa :: Char -> Bool
isAlfa c = (ascii > 64 && ascii < 91) || (ascii > 96 && ascii < 123)
    where ascii = fromEnum c

isDigit :: Char -> Bool
isDigit c = c `elem` "0123456789"


--Since we couldn't figure out how we had to make a correct parser given the grammer we make a simplified version.
--It does not return an Expr but a string of the expression that can be inputted into the datatype
parse :: String -> String
parse [] = ""
parse (s:ss)
    | isDigit s == True = "(Val " ++ (s:ss) ++ ")"
    | isAlfa s == True = "(Var " ++ (s:ss) ++ ")"
    | isOperator s == True = " :" ++ (s:ss) ++ ": "
        where
             isOperator :: Char -> Bool
             isOperator c = c `elem` "+-*/%"

toExpr :: String -> String
toExpr s = "(" ++ doIt (tokenize s) ++ ")"
        where
             doIt :: [String] -> String
             doIt (s:[]) = (parse s)
             doIt (s:ss) = (parse s) ++ (doIt ss)
