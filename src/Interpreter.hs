module Interpreter
  ( Expr (..)
  , emptyEnv
  , eval
  )
  where
import qualified Data.Map as Map

data Expr
  = Const Int
  | Add Expr Expr
  | Var String
  | Let String Expr Expr
  deriving Show

type Env = Map.Map String Int

emptyEnv :: Env
emptyEnv = Map.empty

eval :: Env -> Expr -> Int
eval env expr =
  case expr of
    Const i -> i
    Add e1 e2 -> (eval env e1) + (eval env e2)
    Var name -> env Map.! name
    Let varName varExpr letExpr ->
      let
        varValue = eval env varExpr
        newEnv = Map.insert varName varValue env
      in
        eval newEnv letExpr