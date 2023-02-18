module NamelessSpec where

import Test.Hspec
import Nameless

spec :: Spec
spec = do
  describe "Nameless tests" $ do
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
      ( Let ( Const 14 )
        ( Let ( Const (-15) )
          ( Add 
            ( Var 0 )
            ( Add
              ( Var 1 )
              ( Var 0 )
            )
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