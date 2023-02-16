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
      Node ("a", 1)
        ( Node ("b", -1) Null Null )
        ( Node ("c", 3)
          ( Node ("d", 4) Null Null )
          Null
        )
    result = search tree "c"
  in
    maybe "not found" show result