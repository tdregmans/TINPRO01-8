{--
    huffmandecompress.hs
    TINPRO01-8 (Functional programming 2)
    Sep van der Biezen, Thijs Dregmans 
    Last edited: 2023-05-11
--}

module Main where

import System.Environment


data Codetree a = Branchy Int (Codetree a) (Codetree a)
                | Branchy2 Int Char
                deriving (Show, Eq, Ord, Read)

split :: Codetree a -> [Char] -> (Char, [Char])
split (Branchy2 a b) c = (b, c)
split (Branchy a b c) d
  | head d == '1' = split b rest
  | otherwise = split c rest
  where rest = drop 1 d

decompress :: Codetree a -> [Char] -> [Char]
decompress _ [] = []
decompress a b = fst c : decompress a (snd c)
  where c = split a b


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