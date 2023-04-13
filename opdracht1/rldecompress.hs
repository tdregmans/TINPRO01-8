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

-- check :: String -> String
-- check x
--     | length x > 1 = show (length x) ++ nub x
--     | otherwise = x

check :: String -> Bool
check (x:xs:xss)
    | isDigit x && isDigit xs = 
    | isDigit x = 
    | otherwise = x

specialGroup x = reverse (splitPlaces)

decompress :: String -> String
decompress input = concat (map check (group input))



main = do [sourcefile, targetfile] <- getArgs
          filecontent <- readFile sourcefile

          let uncompressedContent = decompress filecontent
          let lenSource = length filecontent
          let lenDecompressed = length uncompressedContent
          let factor = round ((fromIntegral lenDecompressed) / (fromIntegral lenSource) * 100)
          
          
          putStrLn $ "length of " ++ sourcefile ++ ": " ++ show lenSource ++ " characters"
          putStrLn $ "length of decompressed file " ++ targetfile ++ ": " ++ show lenDecompressed ++ " characters"
          putStrLn $ "factor " ++ show lenDecompressed ++ "/" ++ show lenSource ++ "*100=" ++ show factor ++ "%"
          
          writeFile targetfile uncompressedContent
          putStrLn "done..."