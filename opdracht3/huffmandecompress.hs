{--
    huffmandecompress.hs
    TINPRO01-8 (Functional programming 2)
    Sep van der Biezen, Thijs Dregmans 
    Last edited: 2023-05-09
--}

module Main where

import System.Environment


data Codetree a = Branchy Int (Codetree a) (Codetree a)
                | Branchy2 Int Char
                deriving (Show, Eq, Ord, Read)

decompress :: Codetree a -> Codetree a -> [Char] -> [Char] -> [Char]
decompress _ _ [] result = result
decompress main (Branchy2 a b) xs result = decompress main main xs (result++[b]) 
decompress main (Branchy a b c) (x:xs) result
  | x == '1' = decompress main b xs result
  | otherwise = decompress main c xs result


main = do [sourcefile, targetfile, codetreefile] <- getArgs
-- Do not forget to read the file in binary, and not a string with 0's and 1's!
          filecontent <- readFile sourcefile

          strCodetree <- readFile codetreefile
          let codetree = (read strCodetree :: Codetree a)
          -- putStrLn (show codetree)

          let uncompressedContent = decompress codetree codetree filecontent []

          let lenUncompressed = length uncompressedContent

-- Note: Assumption: 1 byte is 8 bits.
          putStrLn $ "length of decompressed file: " ++ show lenUncompressed ++ " characters, " ++ show (lenUncompressed  * 8) ++ " bits."
          

          writeFile targetfile uncompressedContent
          putStrLn $ targetfile ++ " written to disk..."

          putStrLn "done"