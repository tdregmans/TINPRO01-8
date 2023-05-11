{--
    huffmandecompress.hs
    TINPRO01-8 (Functional programming 2)
    Sep van der Biezen, Thijs Dregmans 
    Last edited: 2023-05-09
--}

module Main where

import System.Environment
import Data.ByteString.Lazy as BS
import Data.Word
import Data.Bits

import Data.List
import Data.List.Split

import Prelude as P

data Codetree a = Branchy Int (Codetree a) (Codetree a)
                | Branchy2 Int Char
                | Empty
                deriving (Show, Eq, Ord)

example :: String
example = "110110110101001010000111011100010100001110111100011011011011011010101010"

example2 :: Codetree a 
example2 = Branchy 34 (Branchy 22 (Branchy 12 (Branchy 4 (Branchy2 1 'e') (Branchy2 3 'd')) (Branchy2 8 'a')) (Branchy2 10 'b')) (Branchy2 12 'c')

test :: String -> [String]
test input = P.zip (Data.List.repeat '0') (splitOn "0" input)

main = do [sourcefile, targetfile, codetreefile] <- getArgs
-- Do not forget to read the file in binary, and not a string with 0's and 1's!
          filecontent <- P.readFile sourcefile
          codetree <- P.readFile codetreefile

          let uncompressedContent = ""-- add functions

          let lenUncompressed = P.length uncompressedContent

-- Note: Assumption: 1 byte is 8 bits.
          putStrLn $ "length of decompressed file: " ++ show lenUncompressed ++ " characters, " ++ show (lenUncompressed * 8) ++ " bits."
          

          P.writeFile targetfile uncompressedContent
          putStrLn $ targetfile ++ " written to disk..."

          putStrLn "done"