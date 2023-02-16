module BinaryTree
  ( Tree
  , search
  , insert
  , erase)
  where

data Tree
  = Node int Tree Tree
  | Null
  derive Show

search :: Tree -> int -> Maybe int
search tree target =
  case tree of
    Node value left right ->
      if value == target then
        Some value
      else
        (search left) >>=
          \ret ->
            case ret of
              Some result -> Some result
              None -> search right
    Null -> None

insert :: Tree -> int -> Tree
insert tree item =
  case tree of
    Node value left right ->
      if item == value then
        tree
      else if item < value then
        Node
          value 
          (insert left item)
          right
      else -- if item > value then
        Node
          value
          left
          (insert right item)
    Null -> 
      Node item Null Null

erase :: Tree -> int -> Tree
erase tree item =
  case tree of
    Node value left right ->
      if item == value then
        let
          (poped, leaf) = popMost left
        in
          Node
            (valueOf leaf)
            poped
            right
      else if item < value then
        Node
          value 
          (erase left)
          right
      else -- if item > value then
        Node
          value
          left
          (erase right)
    Null -> Null

popMost :: Tree -> Maybe int
popMost tree =
  case tree of
    Node value _ right ->
      let
        ret = popMost right
      in
        case ret of
          Some result -> ret
          None -> Some value
    Null -> None
