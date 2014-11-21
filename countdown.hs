module Countdown where

data Op = Add | Sub | Mult | Div deriving Show
--instance Show Op where
--	show Add = " + "
--	show Sub = " - "
--	show Mult = " * "
--	show Div = " / "

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mult x y = x * y
apply Div x y = x `div` y

valid :: Op -> Int -> Int -> Bool
valid Add x y = x <= y
valid Sub x y = x > y
valid Mult x y = x <= y && x /= 1 && y /= 1
valid Div x y = x `mod` y == 0 && y /= 1

data Expr = Val Int | App Op Expr Expr deriving Show
--instance Show Expr where
--	show (Val x) = show x
--	show (App o l r) = "(" ++ show l ++ show o ++ show r ++ ")"

type Result = (Expr, Int) 

eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l, y <- eval r, valid o x y]

subs                          :: [a] -> [[a]]
subs []                       =  [[]]
subs (x:xs)                   =  yss ++ map (x:) yss
                                 where yss = subs xs

interleave                    :: a -> [a] -> [[a]]
interleave x []               =  [[x]]
interleave x (y:ys)           =  (x:y:ys) : map (y:) (interleave x ys)

perms                         :: [a] -> [[a]]
perms []                      =  [[]]
perms (x:xs)                  =  concat (map (interleave x) (perms xs))

choices                       :: [a] -> [[a]]
choices xs                    =  [zs | ys <- subs xs, zs <- perms ys]

values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]

split :: [a] -> [([a],[a])]
split [] = []
split [_] = []
split (x:xs) = ([x],xs) : [(x:ls,rs) | (ls,rs) <- split xs]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls,rs) <- split ns
				, l <- exprs ls
				, r <- exprs rs
				, e <- combine l r]
				
combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- [Add, Sub, Mult, Div]]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns
					, e <- exprs ns'
					, eval e == [n]]

combine' :: Result -> Result -> [Result]
combine' (l,x) (r,y) = [(App o l r, apply o x y) | o <- [Add, Sub, Mult, Div], valid o x y]
					
results :: [Int] -> [Result]
results [] = []
results [n] =[(Val n, n) | n > 0]
results ns = [res | (ls,rs) <- split ns
				  , lx <- results ls
				  , ry <- results rs
				  , res <- combine' lx ry]
				  
solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [e | ns' <- choices ns
					 , (e,m) <- results ns'
					 , m == n]
					
op_pprint :: Op -> String
op_pprint Add = "+"
op_pprint Sub = "-"
op_pprint Mult = "*"
op_pprint Div = "/"
					
pprint :: Expr -> String
pprint (Val x) = show x
pprint (App o l r) = "(" ++ pprint l ++ op_pprint o ++ pprint r ++ ")"
					