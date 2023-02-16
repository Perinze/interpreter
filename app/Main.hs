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
        ( Node (-1) Null Null )
        ( Node 3
          ( Node 2 Null Null )
          Null
        )
    result = search tree 3
  in
    maybe "not found" show result