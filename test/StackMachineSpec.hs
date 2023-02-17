module StackMachineSpec where

import Test.Hspec
import StackMachine

spec :: Spec
spec = do
  describe "Stack machine tests" $ do
    it "const" $
      shouldBe
        ( exec [ Const 1 ] [] )
        1
    
    it "add" $
      shouldBe
        ( exec
          [ Const 1
          , Const 2
          , Add
          ]
          []
        )
        3