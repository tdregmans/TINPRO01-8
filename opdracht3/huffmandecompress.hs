{--
    huffmandecompress.hs
    TINPRO01-8 (Functional programming 2)
    Sep van der Biezen, Thijs Dregmans 
    Last edited: 2023-04-21
--}

module Main where

import System.Environment

main = do [sourcefile, targetfile, codetreefile] <- getArgs
-- Do not forget to read the file in binary, and not a string with 0's and 1's!
          filecontent <- readFile sourcefile
          codetree <- readFile codetreefile

          let uncompressedContent = ""-- add functions

          let lenUncompressed = length uncompressedContent

-- Note: Assumption: 1 byte is 8 bits.
          putStrLn $ "length of decompressed file: " ++ show lenUncompressed ++ " characters, " ++ show (lenUncompressed * 8) ++ " bits."
          

          writeFile targetfile uncompressedContent
          putStrLn $ targetfile ++ " written to disk..."

          putStrLn "done"