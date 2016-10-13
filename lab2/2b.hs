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
