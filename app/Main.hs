module Main where

import BinaryTree

--main :: IO ()
--main = do
--  putStrLn $ eval Cst 1
--  putStrLn $ eval Add (Cst 1) (Cst 2)
--  putStrLn $ eval
--    Add
--      (Add (Cst 1) (Cst 2))
--      (Add (Cst 4) (Cst 5))

main :: IO ()
main = putStrLn test

test :: String
test =
  let
    tree =
      Node 0
        Node -1 Null Null
        Node 3
          Node 2 Null Null
          Null
        Node 5
          Node 4 Null Null
          Node 6 Null Null
    result = search tree 4
  in
    case result of
      Some value -> show value
      None -> "not found"