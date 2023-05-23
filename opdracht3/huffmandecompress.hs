{--
    huffmandecompress.hs
    TINPRO01-8 (Functional programming 2)
    Sep van der Biezen, Thijs Dregmans 
    Last edited: 2023-05-23
--}

module Main where

import System.Environment

data Codetree a = Branchy Int (Codetree a) (Codetree a)
                | Branchy2 Int Char
                deriving (Show, Eq, Ord, Read)

split :: Codetree a -> [Char] -> (Char, [Char])
split (Branchy2 _ chr) bits = (chr, bits)
split (Branchy _ tree1 tree2) bits
  | head bits == '1' = split tree1 rest
  | otherwise = split tree2 rest
  where rest = drop 1 bits

decompress :: Codetree a -> [Char] -> [Char]
decompress _ [] = []
decompress tree bitString = fst tuple : decompress tree (snd tuple)
  where tuple = split tree bitString
  -- tuple is the (first char, [rest of the bitsco]) 


main = do [sourcefile, targetfile, codetreefile] <- getArgs
          filecontent <- readFile sourcefile

          strCodetree <- readFile codetreefile
          let codetree = (read strCodetree :: Codetree a)

          let uncompressedContent = decompress codetree filecontent

          let lenUncompressed = length uncompressedContent

          -- Note: Assumption: 1 byte is 8 bits.
          putStrLn $ "length of decompressed file: " ++ show lenUncompressed ++ " characters, " ++ show (lenUncompressed  * 8) ++ " bits."
          
          writeFile targetfile uncompressedContent
          putStrLn $ targetfile ++ " written to disk..."

          putStrLn "done"