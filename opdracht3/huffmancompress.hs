{--
    huffmancompress.hs
    TINPRO01-8 (Functional programming 2)
    Sep van der Biezen, Thijs Dregmans 
    Last edited: 2023-05-09
--}

-- Do not forget to sterialize the codetree before storing -> see ../opdracht2/opdracht2.hs

module Main where

import Data.List
import System.Environment

import Data.Functor

-- example to test functions
example :: String
example = "aaabbcbbcccddccbbcccdecccaaaaabbbb"
-- example can be deleted when assignment is completed

-- function for implementing Huffman compression step 1
huffmanStep1 :: String -> [(Int, Char)]
huffmanStep1 str = reverse (sortOn fst (map (\str -> (length str, head str) ) (group (sort str))))

data Codetree a = Branchy Int (Codetree a) (Codetree a)
                | Branchy2 Int Char
                | Empty
                deriving (Show, Eq, Ord)

val :: Codetree a -> Int
val (Branchy a _ _) = a
val (Branchy2 a _) = a
val Empty = 0

-- function for implementing Huffman compression step 2
prepHuffmanStep2 :: [(Int, Char)] -> [Codetree a]
prepHuffmanStep2 = map (\x -> Branchy2 (fst x) (snd x))

huffmanStep2 :: [Codetree a] -> Codetree a -> Codetree a
huffmanStep2 [] tree = tree
huffmanStep2 [x] tree = tree
huffmanStep2 treeList tree = huffmanStep2 (tail (tail list) ++ [newTree]) newTree 
  where list = sort treeList

        first = head list
        secnd = head (tail list)

        newTree = Branchy (val first + val secnd) first secnd

-- function for implementing Huffman compression step 3
huffmanStep3 :: Codetree a -> [(Char, Int)]
huffmanStep3 table = [('a', 1)] -- not completed


-- function for implementing Huffman compression step 4
huffmanStep4 :: String -> [(Char, Int)] -> String
huffmanStep4 str table = str -- not completed


main = do [sourcefile, targetfile, codetreefile] <- getArgs
          filecontent <- readFile sourcefile

          let codetree = huffmanStep2 (prepHuffmanStep2 (huffmanStep1 filecontent)) Empty
          let compressedContent = huffmanStep4 filecontent (huffmanStep3 codetree)

          let lenSource = length filecontent
          let lenCompressed = length compressedContent
          let factor = round (fromIntegral lenCompressed / fromIntegral lenSource * 100)

-- Note: Assumption: 1 byte is 8 bits.
          putStrLn $ "length of " ++ sourcefile ++ ": " ++ show lenSource ++ " characters, " ++ show (lenSource * 8) ++ " bits."
          putStrLn $ "length of compressed file " ++ targetfile ++ ": " ++ show lenCompressed ++ " bits."
-- Do not forget to write the file in binary, and not a string with 0's and 1's!
          putStrLn $ "factor " ++ show lenCompressed ++ "/" ++ show lenSource ++ "*100=" ++ show factor ++ "%"
          

          writeFile targetfile compressedContent
          putStrLn $ targetfile ++ " written to disk..."

          writeFile codetreefile (show codetree)
          putStrLn $ show codetree ++ "written to disk..."

          putStrLn "done"