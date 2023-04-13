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
  | length x == 1 = x
  | otherwise = replicate n (x !! index) ++ decompress (drop (index+1) x)
    where index = length (takeWhile isDigit x)
          strNum = take index x
          n = if strNum == "" then 1 else read strNum :: Int

main = do [sourcefile, targetfile] <- getArgs
          filecontent <- readFile sourcefile

          let uncompressedContent = decompress filecontent

          writeFile targetfile uncompressedContent
          putStrLn "done..."