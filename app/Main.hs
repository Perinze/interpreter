module Main where
import Interpreter

main :: IO ()
main = do
  putStrLn . evalStr $
    Const 1

  putStrLn . evalStr $
    Add (Const 1) (Const 2)

  putStrLn . evalStr $
    Add
      (Add (Const 1) (Const 2))
      (Add (Const 4) (Const 5))

  putStrLn . evalStr $
    Let "x" (Add (Const 1) (Const 2))
      (Add (Var "x") (Var "x"))

evalStr :: Expr -> String
evalStr = show . eval emptyEnv