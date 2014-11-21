module Countdown where

data Op = Add | Sub | Mult | Div deriving Show
data Expr = Val Int | App Op Expr Expr deriving Show
type Result = (Expr, Int) 

-- How do we apply an Operator to two Arguments
apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mult x y = x * y
apply Div x y = x `div` y

-- How do we know a result behaves per the rules of the game (>= 0)
valid :: Op -> Int -> Int -> Bool
valid Add x y = x <= y
valid Sub x y = x > y
valid Mult x y = x <= y && x /= 1 && y /= 1
valid Div x y = x `mod` y == 0 && y /= 1

-- Evaluate an expression and return its result (or [])
eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l, y <- eval r, valid o x y]

-- choices helpers
subs :: [a] -> [[a]]
subs [] =  [[]]
subs (x:xs) =  yss ++ map (x:) yss
				where yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x [] =  [[x]]
interleave x (y:ys) =  (x:y:ys) : map (y:) (interleave x ys)

perms :: [a] -> [[a]]
perms [] =  [[]]
perms (x:xs) =  concat (map (interleave x) (perms xs))

-- All permutations of the powerset of the input list
choices                       :: [a] -> [[a]]
choices xs                    =  [zs | ys <- subs xs, zs <- perms ys]

-- The list of Values in an Expression
values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

-- One possible solution
solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]

-- Splits a list at all possible spots
split :: [a] -> [([a],[a])]
split [] = []
split [_] = []
split (x:xs) = ([x],xs) : [(x:ls,rs) | (ls,rs) <- split xs]
	
-- 	Combine two results using all possible operators				
combine' :: Result -> Result -> [Result]
combine' (l,x) (r,y) = [(App o l r, apply o x y) | o <- [Add, Sub, Mult, Div], valid o x y]
					
-- Get all Results from list of Int values					
results :: [Int] -> [Result]
results [] = []
results [n] =[(Val n, n) | n > 0]
results ns = [res | (ls,rs) <- split ns
				  , lx <- results ls
				  , ry <- results rs
				  , res <- combine' lx ry]
				  
-- Find the Solution set				  
solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [e | ns' <- choices ns
					 , (e,m) <- results ns'
					 , m == n]
					
-- Pretty Print functions					
op_pprint :: Op -> String
op_pprint Add = "+"
op_pprint Sub = "-"
op_pprint Mult = "*"
op_pprint Div = "/"
					
pprint :: Expr -> String
pprint (Val x) = show x
pprint (App o l r) = "(" ++ pprint l ++ op_pprint o ++ pprint r ++ ")"
					