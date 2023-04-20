{--
    opdracht2.hs
    TINPRO01-8 (Functional programming 2)
    Sep van der Biezen, Thijs Dregmans 
    Last edited: 2023-04-19
--}


-- Source: https://en.wikipedia.org/wiki/Binary_search_tree
-- Source: https://en.wikipedia.org/wiki/Tree_traversal

module Main where

import System.Environment

-- Opdracht 2.1
data Bintree a = Empty
               | Branch a (Bintree a) (Bintree a)
               deriving (Show, Read)

-- Opdracht 2.2
preorder :: (Bintree a) -> [a]
preorder Empty = []
preorder (Branch a b c) = [a] ++ preorder b ++ preorder c
-- return tree in the form of a list

postorder :: (Bintree a) -> [a]
postorder Empty = []
postorder (Branch a b c) = postorder b ++ postorder c ++ [a]
-- return tree in the form of a list

inorder :: (Bintree a) -> [a]
inorder Empty = []
inorder (Branch a b c) = postorder b ++ [a] ++ postorder c
-- return tree in the form of a list

push :: (Ord a) => (Bintree a) -> a -> (Bintree a)
push Empty a = Branch a Empty Empty
push (Branch a b c) d
  | d < a = Branch a (push b d) c
  | otherwise = Branch a b (push c d)
-- add code to push item in the tree at the right location

pushlist :: (Ord a) => (Bintree a) -> [a] -> (Bintree a)
pushlist = foldl push
-- add code to push items in the tree at the right location

maptree :: (a -> b) -> (Bintree a) -> (Bintree b)
maptree function Empty = Empty
maptree function (Branch a b c) = Branch (function a) (maptree function b) (maptree function c) 
-- apply function on all elements of tree

filtertree :: (a -> Bool) -> (Bintree a) -> [a]
filtertree bool Empty = []
filtertree bool (Branch a b c) = [a | bool a] ++ filtertree bool b ++ filtertree bool c
-- return filtered nodes


-- Opdracht 3

-- made a start, maybe make two files?

main = do sourcefile <- getArgs
          putStrLn "Hey!"