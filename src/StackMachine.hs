module StackMachine
  ( Instruction (..)
  , Instructions
  , Stack
  , eval
  , Cenv
  , emptyCenv
  , compile
  )
  where
import qualified Nameless

data Instruction
  = Const Int
  | Add
  | Var Int -- push stack[i], where i is indexed from top
  | Pop
  | Swap
  deriving (Show, Eq)

type Instructions = [Instruction]
type Stack = [Int]

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
          
type Cenv = [Int]

emptyCenv :: [Int]
emptyCenv = []

compile :: Cenv -> Nameless.Expr -> Int -> Instructions
compile cenv expr depth =
  let
    comp c e prefix = compile c e (depth + prefix)
  in
    case expr of
      Nameless.Const i ->
        [ Const i ]
      Nameless.Add e1 e2 ->
        ( comp cenv e1 0 )
        ++ ( comp cenv e2 1 )
        ++ [ Add ]
      Nameless.Var n ->
        let
          varDepth = cenv !! n
          varOffset = depth - varDepth - 1
        in
          [ Var varOffset ]
      Nameless.Let varExpr letExpr ->
        let
          newCenv = cenv ++ [ depth ]
        in
          ( comp cenv varExpr 0 )
          ++ ( comp newCenv letExpr 1 )
          ++ [ Swap, Pop ]