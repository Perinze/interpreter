module StackMachineSpec where

import Test.Hspec
import StackMachine
import qualified Nameless

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

    it "compile const" $
      shouldBe
        ( compile
          emptyCenv
          ( Nameless.Const 1 )
          0
        )
        [ Const 1 ]

    it "compile add" $
      shouldBe
        ( compile
          emptyCenv
          ( Nameless.Add
            ( Nameless.Const 1 )
            ( Nameless.Add
              ( Nameless.Const 2 )
              ( Nameless.Const 3 )
            )
          )
          0
        )
        [ Const 1
        , Const 2
        , Const 3
        , Add
        , Add
        ]
    
    it "compile let var add" $
      shouldBe
        ( compile
          emptyCenv
          ( Nameless.Let
            ( Nameless.Const 1 )
            ( Nameless.Let
              ( Nameless.Add
                ( Nameless.Var 0 )
                ( Nameless.Var 0 )
              )
              ( Nameless.Add
                ( Nameless.Var 0 )
                ( Nameless.Add
                  ( Nameless.Const 1 )
                  ( Nameless.Var 1 )
                )
              )
            )
          )
          0
        )
        [ Const 1
        , Var 0
        , Var 1
        , Add
        , Var 1
        , Const 1
        , Var 2
        , Add
        , Add
        , Swap
        , Pop
        , Swap
        , Pop
        ]
