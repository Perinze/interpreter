module InterpreterSpec where

import Test.Hspec
import Interpreter

spec :: Spec
spec = do
  describe "Interpreter tests" $ do
    evalTest "const"
      ( Const 1 )
      1

    evalTest "add"
      ( Add
        ( Const 1 )
        ( Const 2 )
      )
      3


evalTest :: String -> Expr -> Int -> Int
evalTest name expr expect =
  it name $
    shouldBe
      ( eval
        emptyEnv
        expr
      )
      expect