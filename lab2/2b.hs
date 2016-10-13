type Name = String
type Domain = [Integer]

data Expr = Val Integer
    | Var Name
    | Expr :+: Expr
    | Expr :-: Expr
    | Expr :*: Expr
    | Expr :/: Expr
    | Expr :%: Expr

par :: String -> String
par s = "(" ++ s ++ ")"

instance Show Expr where
    show (Var x) = x
    show (Val y) = show y
    show (p :+: q) = par(show p ++ "+" ++ show q)
    show (p :-: q) = par(show p ++ "-" ++ show q)
    show (p :*: q) = par(show p ++ "*" ++ show q)
    show (p :/: q) = par(show p ++ "/" ++ show q)
    show (p :%: q) = par(show p ++ "%" ++ show q)

vars :: Expr -> [Name]
vars (Var x) = [x]
vars (Val y) = []
vars (p :+: q) = uniq(vars p ++ vars q)
vars (p :-: q) = uniq(vars p ++ vars q)
vars (p :*: q) = uniq(vars p ++ vars q)
vars (p :/: q) = uniq(vars p ++ vars q)
vars (p :%: q) = uniq(vars p ++ vars q)

uniq :: Ord a => [a] -> [a]
uniq [] = []
uniq (x:xs) = sortList(x:uniq(filter (/= x) xs))
    where sortList :: Ord a => [a] -> [a]
          sortList [] = []
          sortList (x:[]) = [x]
          sortList (x1:x2:xs)
              | x1 < x2 = x1 : sortList (x2:xs)
              | otherwise = x2 : sortList (x1:xs)


type Valuation = [(Name, Integer)]

evalExpr :: Expr -> Valuation -> Integer
evalExpr e v
    | (verify e v) == True = evalExpr e v
    | otherwise            = error
evalExpr (Var x) v = sure(lookup x v) where sure (Just b) = b
evalExpr (Val y) v = y
evalExpr (p :+: q) v = evalExpr p v + (evalExpr q v)
evalExpr (p :-: q) v = evalExpr p v - (evalExpr q v)
evalExpr (p :*: q) v = evalExpr p v * (evalExpr q v)
evalExpr (p :/: q) v = evalExpr p v `div` (evalExpr q v)
evalExpr (p :%: q) v = evalExpr p v `mod` (evalExpr q v)


-- empty valuation eventhough given variable: (Var "a") [] or (Var "a" + Var "b") [("a", 2)]
-- declared as Var but not of the type variable: (Var 2)
-- variable name has other than number in m: (Var "a1")
-- empty Val: (Val :+: Val)
-- declared as Val but not of type value: (Val "a")
verify :: Expr -> Valuation -> Bool
verify (Var x) [] = False
verify (Var x) _  = checkIt x
verify (Val y) _ = checkIt [y]
verify (p :+: q) v = verify p v && verify q v
verify (p :-: q) v = verify p v && verify q v
verify (p :*: q) v = verify p v && verify q v
verify (p :/: q) v = verify p v && verify q v
verify (p :%: q) v = verify p v && verify q v

checkIt :: [a] -> Bool
checkIt [] = False
checkIt _  = True
