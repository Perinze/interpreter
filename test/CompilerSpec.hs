module CompilerSpec where

import Test.Hspec
import qualified Lang
import Compiler
import qualified StackMachine

spec :: Spec
spec = do
  describe "Compiler tests" $ do
    it "const" $
      shouldBe
        ( compile ( Lang.Const 1 ) [] )
        [ StackMachine.Const 1 ]
    
    it "add" $
      shouldBe
        ( compile
          ( Lang.Add
            ( Lang.Const 1 )
            ( Lang.Const 2 )
          )
          []
        )
        [ StackMachine.Const 1
        , StackMachine.Const 2
        , StackMachine.Add
        ]