main :: IO ()
main = do
  putStrLn $ eval Cst 1
  putStrLn $ eval Add (Cst 1) (Cst 2)
  putStrLn $ eval
    Add
      (Add (Cst 1) (Cst 2))
      (Add (Cst 4) (Cst 5))