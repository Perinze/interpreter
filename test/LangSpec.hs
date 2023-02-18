module LangSpec where

import Test.Hspec
import Lang

spec :: Spec
spec = do
  describe "Lang tests" $ do
    evalTest "const"
      ( Const 1 )
      1

    evalTest "add"
      ( Add
        ( Const 1 )
        ( Const 2 )
      )
      3
    
    evalTest "let"
      ( Let "x" ( Const 14 )
        ( Add 
          ( Var "x" )
          ( Add
            ( Const (-15) )
            ( Var "x")
          )
        )
      )
      13

evalTest :: String -> Expr -> Int -> SpecWith ()
evalTest name expr expect =
  it name $
    shouldBe
      ( eval
        emptyEnv
        expr
      )
      expect