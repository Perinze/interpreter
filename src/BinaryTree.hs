{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module BinaryTree
  ( Tree (..)
  , search
  , insert
  , erase)
  where
import Data.Maybe (fromMaybe)

data Tree
  = Node Entry Tree Tree
  | Null
  deriving Show

type Entry
  = (String, Int)

search :: Tree -> String -> Maybe Int
search tree target =
  case tree of
    Node (key, value) left right ->
      if key == target then
        Just value
      else
        let
          ret = search left target
        in
          case ret of
            Just _ -> ret
            Nothing -> search right target
    Null -> Nothing

insert :: Tree -> Entry -> Tree
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

erase :: Tree -> String -> Tree
erase tree item =
  case tree of
    Node (key, value) left right ->
      if item == key then
        let
          -- Just v = most tree
          v = fromMaybe ("", 0) (most tree)
          poped = popLeaf tree v
        in
          Node
            v
            poped
            right
      else if item < key then
        Node
          (key, value) 
          (erase left item)
          right
      else -- if item > key then
        Node
          (key, value)
          left
          (erase right item)
    Null -> Null

most :: Tree -> Maybe Entry
most tree =
  case tree of
    Node entry _ right ->
      let
        ret = most right
      in
        case ret of
          Just _ -> ret
          Nothing -> Just entry
    Null -> Nothing

popLeaf :: Tree -> Entry -> Tree
popLeaf tree item =
  case tree of
    Node value Null Null ->
      if item == value then
        Null
      else
        tree
    _ -> tree

valueOf :: Tree -> Maybe Entry
valueOf tree =
  case tree of
    Node value _ _ -> Just value
    Null -> Nothing