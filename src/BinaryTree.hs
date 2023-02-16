module BinaryTree
  ( Tree (..)
  , search
  , insert
  , erase)
  where
import Data.Maybe (fromMaybe)

data Tree
  = Node Int Tree Tree
  | Null
  deriving Show

search :: Tree -> Int -> Maybe Int
search tree target =
  case tree of
    Node value left right ->
      if value == target then
        Just value
      else
        case search left target of
            Just result -> Just result
            Nothing -> search right target
    Null -> Nothing

insert :: Tree -> Int -> Tree
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

erase :: Tree -> Int -> Tree
erase tree item =
  case tree of
    Node value left right ->
      if item == value then
        let
          -- Just v = most tree
          v = fromMaybe 0 (most tree)
          poped = popLeaf tree v
        in
          Node
            v
            poped
            right
      else if item < value then
        Node
          value 
          (erase left item)
          right
      else -- if item > value then
        Node
          value
          left
          (erase right item)
    Null -> Null

most :: Tree -> Maybe Int
most tree =
  case tree of
    Node value _ right ->
      let
        ret = most right
      in
        case ret of
          Just _ -> ret
          Nothing -> Just value
    Null -> Nothing

popLeaf :: Tree -> Int -> Tree
popLeaf tree item =
  case tree of
    Node value Null Null ->
      if item == value then
        Null
      else
        tree
    _ -> tree

valueOf :: Tree -> Maybe Int
valueOf tree =
  case tree of
    Node value _ _ -> Just value
    Null -> Nothing