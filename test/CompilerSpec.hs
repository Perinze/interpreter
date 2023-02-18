module CompilerSpec where

import Test.Hspec
--import qualified Lang
--import qualified StackMachine
--
spec :: Spec
spec = do
  describe "Compiler tests" $ do
    it "pass" $
      shouldBe "pass" "pass"
--    it "const" $
--      shouldBe
--        ( compile ( Lang.Const 1 ) [] )
--        [ StackMachine.Const 1 ]
--    
--    it "add" $
--      shouldBe
--        ( compile
--          ( Lang.Add
--            ( Lang.Const 1 )
--            ( Lang.Const 2 )
--          )
--          []
--        )
--        [ StackMachine.Const 1
--        , StackMachine.Const 2
--        , StackMachine.Add
--        ]