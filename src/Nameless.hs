module Nameless
  ( Cenv
  , Expr (..)
  , emptyEnv
  , eval
  )
  where
  
type Cenv = [Int]

emptyEnv :: Cenv
emptyEnv = []

data Expr
  = Const Int
  | Add Expr Expr
  | Var Int
  | Let Expr Expr

eval :: Cenv -> Expr -> Int
eval env expr =
  case expr of
    Const i -> i
    Add e1 e2 -> (eval env e1) + (eval env e2)
    Var n -> env !! n
    Let varExpr letExpr ->
      let
        varValue = eval env varExpr
        newEnv = env ++ [varValue]
      in
        eval newEnv letExpr
