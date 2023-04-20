{--
    opdracht2.hs
    TINPRO01-8 (Functional programming 2)
    Sep van der Biezen, Thijs Dregmans 
    Last edited: 2023-04-20
--}

-- Source: https://en.wikipedia.org/wiki/Binary_search_tree
-- Source: https://en.wikipedia.org/wiki/Tree_traversal

module Main where

import Data.Char
import Data.List
import System.Environment

-- Opdracht 2.1

data Bintree a = Empty
               | Branch a (Bintree a) (Bintree a)
               deriving (Show, Read)

-- Opdracht 2.2

preorder :: (Bintree a) -> [a]
preorder Empty = []
preorder (Branch a b c) = [a] ++ preorder b ++ preorder c

postorder :: (Bintree a) -> [a]
postorder Empty = []
postorder (Branch a b c) = postorder b ++ postorder c ++ [a]

inorder :: (Bintree a) -> [a]
inorder Empty = []
inorder (Branch a b c) = postorder b ++ [a] ++ postorder c

push :: (Ord a) => (Bintree a) -> a -> (Bintree a)
push Empty a = Branch a Empty Empty
push (Branch a b c) d
  | d < a = Branch a (push b d) c
  | otherwise = Branch a b (push c d)

pushlist :: (Ord a) => (Bintree a) -> [a] -> (Bintree a)
pushlist = foldl push

maptree :: (a -> b) -> (Bintree a) -> (Bintree b)
maptree function Empty = Empty
maptree function (Branch a b c) = Branch (function a) (maptree function b) (maptree function c)

filtertree :: (a -> Bool) -> (Bintree a) -> [a]
filtertree bool Empty = []
filtertree bool (Branch a b c) = [a | bool a] ++ filtertree bool b ++ filtertree bool c

-- Opdracht 2.3

main = do [sourcefile] <- getArgs
          -- 1
          filecontent <- readFile sourcefile
          -- 2
          let tree = pushlist Empty filecontent
          -- 3
          let intTree = maptree ord tree
          -- 4
          writeFile "tree.txt" ( unwords (map show (preorder intTree)))
          -- 5
          filecontent1 <- readFile "tree.txt"
          let tree2 = pushlist Empty (map (read :: String -> Int) (words filecontent1))
          -- 6
          let tree3 = maptree chr tree2
          -- 7
          let string = inorder tree3
          -- 8
          putStrLn string
          -- 9
          let filteredTree = filtertree isDigit tree3
          -- 10
          putStrLn filteredTree

          putStrLn "done"
