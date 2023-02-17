module Compiler
  ( compile
  )
  where

import qualified Lang
import qualified StackMachine

compile :: Lang.Expr -> StackMachine.Instructions
  -> StackMachine.Instructions
compile expr instrs =
  case expr of
    Lang.Const i ->
      instrs ++ [ StackMachine.Const i ]

    Lang.Add e1 e2 -> 
      ( compile e1 instrs )
      ++ ( compile e2 instrs )
      ++ [ StackMachine.Add ]
