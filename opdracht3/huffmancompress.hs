{--
    huffmancompress.hs
    TINPRO01-8 (Functional programming 2)
    Sep van der Biezen, Thijs Dregmans 
    Last edited: 2023-05-23
--}

module Main where

import Data.List
import System.Environment

import Data.Functor

import Control.Applicative

-- example to test functions
example :: String
example = "aaabbcbbcccddccbbcccdecccaaaaabbbb"
-- example can be deleted when assignment is completed

-- function for implementing Huffman compression step 1
huffmanStep1 :: String -> [(Int, Char)]
huffmanStep1 str = reverse (sortOn fst (map (\str -> (length str, head str) ) (group (sort str))))

data Codetree a = Branchy Int (Codetree a) (Codetree a)
                | Branchy2 Int Char
                deriving (Show, Eq, Ord, Read)

value :: Codetree a -> Int
value (Branchy a _ _) = a
value (Branchy2 a _) = a 

-- function for implementing Huffman compression step 2
prepHuffmanStep2 :: [(Int, Char)] -> [Codetree a]
prepHuffmanStep2 tuples = sort [Branchy2 int chr | (int, chr) <- tuples]

huffmanStep2 :: [Codetree a] -> [Codetree a]
huffmanStep2 (x:xs:xss) = huffmanStep2 (sort (Branchy (value x + value xs) x xs : xss))
huffmanStep2 x = x

-- step2 = head (huffmanStep2 (prepHuffmanStep2 (huffmanStep1 example)))

-- function for implementing Huffman compression step 3
huffmanStep3 :: Codetree a -> [Char] -> [([Char], Char)]
huffmanStep3 (Branchy2 int chr) bits = [(bits, chr)]
huffmanStep3 (Branchy int tree1 tree2) bits = huffmanStep3 tree1 (bits++"1") ++ huffmanStep3 tree2 (bits++"0")

-- step3 = sortOn fst (huffmanStep3 step2 [])

chrToBits :: Char -> [([Char], Char)] -> [Char]
chrToBits chr table = fst (head (filter condition table))
  where condition (_, tableChr) = tableChr == chr

huffmanStep4 :: [([Char], Char)] -> [Char] -> [Char]
huffmanStep4 table str = concat [chrToBits chr table | chr <- str]

-- step4 = huffmanStep4 step3 example

main = do [sourcefile, targetfile, codetreefile] <- getArgs
          filecontent <- readFile sourcefile

          let codetree = head (huffmanStep2 (prepHuffmanStep2 (huffmanStep1 filecontent)))
          let compressedContent = huffmanStep4 (sortOn fst (huffmanStep3 codetree [])) filecontent

          let lenSource = length filecontent * 8
          let lenCompressed = length compressedContent
          let factor = round (fromIntegral lenCompressed / fromIntegral lenSource * 100)

          putStrLn $ "length of " ++ sourcefile ++ ": " ++ show (lenSource `div` 8) ++ " characters, " ++ show lenSource ++ " bits."
          putStrLn $ "length of compressed file " ++ targetfile ++ ": " ++ show lenCompressed ++ " bits."

          putStrLn $ "factor " ++ show lenCompressed ++ "/" ++ show lenSource ++ "*100=" ++ show factor ++ "%"

          writeFile targetfile compressedContent
          putStrLn $ targetfile ++ " written to disk..."

          writeFile codetreefile (show codetree)
          putStrLn $ show codetree ++ "written to disk..."

          putStrLn "done"
