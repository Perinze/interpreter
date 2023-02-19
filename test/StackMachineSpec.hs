module StackMachineSpec where

import Test.Hspec
import StackMachine

spec :: Spec
spec = do
  describe "Stack machine tests" $ do
    it "const" $
      shouldBe
        ( eval [ Const 1 ] [] )
        1
    
    it "add" $
      shouldBe
        ( eval
          [ Const 1
          , Const 2
          , Add
          ]
          []
        )
        3
    
    it "var" $
      shouldBe
        ( eval
          [ Const 2
          , Const 1
          , Var 1
          , Var 2
          , Add
          , Add
          , Add
          ]
          []
        )
        7

    it "pop" $
      shouldBe
        ( eval
          [ Const 1
          , Const 4
          , Pop
          , Const 5
          , Const 2
          , Const 4
          , Add
          , Pop
          , Add
          ]
          []
        )
        6

    it "swap" $
      shouldBe
        ( eval
          [ Const 1
          , Const 4
          , Pop
          , Const 5
          , Const 2
          , Const 4
          , Add
          , Swap
          , Pop
          , Add
          ]
          []
        )
        7