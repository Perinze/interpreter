module Lang
  ( Expr (..)
  )
  where

data Expr
  = Const Int
  | Add Expr Expr
  | Var String
  | Let String Expr Expr
  deriving Show
