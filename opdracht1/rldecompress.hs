{--
    lrdecompress.hs
    TINPRO01-8 (Functional programming 2)
    Sep van der Biezen, Thijs Dregmans 
    Last edited: 2023-04-13
--}

module Main where

import Data.List
import Data.Char
import System.Environment
import Text.Read

decompress :: [Char] -> [Char]
decompress x
  | null x = []
  | index == 0 = take 1 x ++ rest
  | otherwise = replicate (read (take index x) :: Int) (x !! index) ++ rest
    where index = length (takeWhile isDigit x)
          rest = decompress (drop (index+1) x)

main = do [sourcefile, targetfile] <- getArgs
          filecontent <- readFile sourcefile

          let uncompressedContent = decompress filecontent

          writeFile targetfile uncompressedContent
          putStrLn "done..."

-- Tip van docent: Gebruik groupBy