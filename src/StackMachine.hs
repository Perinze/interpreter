module StackMachine
  ( Instruction (..)
  , Instructions
  , Stack
  , exec
  )
  where

data Instruction
  = Const Int
  | Add
  deriving Show

type Instructions = [Instruction]
type Stack = [Int]

exec :: Instructions -> Stack -> Int
exec instrs stack =
  case instrs of
    [] -> head stack
    instr : resti ->
      case instr of
        Const i ->
          exec resti (i : stack)
        Add ->
          let
            a : b : rests = stack
            c = a + b
          in
            exec resti (c : rests)