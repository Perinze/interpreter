module StackMachine
  ( Instruction (..)
  , Instructions
  , Stack
  , eval
  )
  where

data Instruction
  = Const Int
  | Add
  | Var Int -- push stack[i], where i is indexed from top
  | Pop
  | Swap
  deriving (Show, Eq)

type Instructions = [Instruction]
type Stack = [Int]

-- TODO: implement eval for Var, Pop and Swap
eval :: Instructions -> Stack -> Int
eval instrs stack =
  case instrs of
    [] -> head stack
    instr : resti ->
      case instr of
        Const i ->
          eval resti (i : stack)
        Add ->
          let
            a : b : rests = stack
            c = a + b
          in
            eval resti (c : rests)
        Var n ->
          let
            v = stack !! n
          in
            eval resti (v : stack)
        Pop ->
          eval resti (tail stack)
        Swap ->
          let
            a : b : rests = stack
          in
            eval resti (b : a : rests)