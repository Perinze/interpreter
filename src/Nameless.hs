module Nameless
  (
  )
  where
  
import qualified Data.List as List

type Cenv = List.List Int

emptyEnv :: Cenv
emptyEnv = List.empty

data Expr

eval :: Cenv -> 