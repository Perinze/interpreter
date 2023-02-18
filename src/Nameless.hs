module Nameless
  ( Env
  , emptyEnv
  , Expr (..)
  , eval
  , Cenv
  , emptyCenv
  , compile
  )
  where

import qualified Lang
import qualified Data.Map as Map
  
type Env = [Int]

emptyEnv :: Env
emptyEnv = []

data Expr
  = Const Int
  | Add Expr Expr
  | Var Int
  | Let Expr Expr

eval :: Env -> Expr -> Int
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

type Cenv = Map.Map String Int

emptyCenv :: Cenv
emptyCenv = Map.empty

compile :: Cenv -> Lang.Expr -> Expr
compile cenv expr =
  case expr of
    Lang.Const i -> Const i
    Lang.Add e1 e2 -> Add (compile cenv e1) (compile cenv e2)
    Lang.Var name -> Var (cenv Map.! name)
    Lang.Let varName varExpr letExpr ->
      let
        newCenv = Map.insert varName (Map.size cenv) cenv
      in
        Let (compile cenv varExpr) (compile newCenv letExpr)
