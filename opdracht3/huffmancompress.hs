{--
    huffmancompress.hs
    TINPRO01-8 (Functional programming 2)
    Sep van der Biezen, Thijs Dregmans 
    Last edited: 2023-04-21
--}

-- Do not forget to sterialize the codetree before storing -> see ../opdracht2/opdracht2.hs

module Main where

import Data.List
import System.Environment

-- example to test functions
example :: String
example = "aaabbcbbcccddccbbcccdecccaaaaabbbb"
-- example can be deleted when assignment is completed

-- function for implementing Huffman compression step 1
huffmanStep1 :: String -> [(Int, Char)]
huffmanStep1 str = reverse (sortOn fst (map (\str -> (length str, head str) ) (group (sort str))))

data Codetree a = Branchy Int (Codetree a) (Codetree a)
                | Branchy2 Int Char -- not completed
                deriving (Show, Eq, Ord)

testy :: [(Int, Char)] -> [Codetree a]
testy c = sort [Branchy2 a b | (a, b) <- c]

getValue :: Codetree a -> Int
getValue (Branchy a _ _) = a
getValue (Branchy2 a _) = a

solvy :: [Codetree a] -> [Codetree a]
solvy (x:xs:xss) = solvy (sort (Branchy (getValue x + getValue xs) x xs : xss))
solvy x = x

-- f = head (solvy (testy (huffmanStep1 example)))

gety :: Codetree a -> [Char] -> [([Char], Char)]
gety (Branchy2 a b) c = [(c, b)]
gety (Branchy a b c) d = gety b (d++"1") ++ gety c (d++"0")

-- g = sortOn fst (gety f [])

getMyBits :: Char -> [([Char], Char)] -> [Char]
getMyBits a b = fst (head (filter condition b))
  where condition (_, t) = t == a

makeArray :: [([Char], Char)] -> [Char] -> [Char]
makeArray a b = concat [getMyBits c a | c<-b]

-- h = makeArray g example

getBackText :: Codetree a -> Codetree a -> [Char] -> [Char] -> [Char]
getBackText _ _ [] result = result
getBackText main (Branchy2 a b) xs result = getBackText main main xs (result++[b]) 
getBackText main (Branchy a b c) (x:xs) result
  | x == '1' = getBackText main b xs result
  | otherwise = getBackText main c xs result

-- i = getBackText f f h []

-- function for implementing Huffman compression step 2
huffmanStep2 :: [(Char, Int)] -> Codetree
huffmanStep2 table = Char 'a' True -- not completed


-- function for implementing Huffman compression step 3
huffmanStep3 :: Codetree -> [(Char, Int)]
huffmanStep3 table = [('a', 1)] -- not completed


-- function for implementing Huffman compression step 4
huffmanStep4 :: String -> [(Char, Int)] -> String
huffmanStep4 str table = str -- not completed


main = do [sourcefile, targetfile, codetreefile] <- getArgs
          filecontent <- readFile sourcefile

          let codetree = huffmanStep2 (huffmanStep1 filecontent)
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

          writeFile codetreefile codetree
          putStrLn $ codetree ++ "written to disk..."

          putStrLn "done"