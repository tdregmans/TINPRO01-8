{--
    lrcompress.hs
    TINPRO01-8 (Functional programming 2)
    Sep van der Biezen, Thijs Dregmans 
    Last edited: 2023-04-13
--}

module Main where

import Data.List

import System.Environment

check :: String -> String
check x
    | length x == 1 = x
    | otherwise = show (length x) ++ [head x]

compress :: String -> String
compress input = concatMap check (group input)



main = do [sourcefile, targetfile] <- getArgs
          filecontent <- readFile sourcefile

          let compressedContent = compress filecontent
          let lenSource = length filecontent
          let lenCompressed = length compressedContent
          let factor = round (fromIntegral lenCompressed / fromIntegral lenSource * 100)


          putStrLn $ "length of " ++ sourcefile ++ ": " ++ show lenSource ++ " characters"
          putStrLn $ "length of compressed file " ++ targetfile ++ ": " ++ show lenCompressed ++ " characters"
          putStrLn $ "factor " ++ show lenCompressed ++ "/" ++ show lenSource ++ "*100=" ++ show factor ++ "%"

          writeFile targetfile compressedContent
          putStrLn "done..."