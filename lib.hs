data expr
  = Const int
  | Add expr expr
  | Var String
  | Let String expr
  derive show

eval :: expr -> int
eval expr =
  case expr of
    Const i -> i
    Add e1 e2 -> (eval e1) + (eval e2)