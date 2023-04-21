{--
    huffmandecompress.hs
    TINPRO01-8 (Functional programming 2)
    Sep van der Biezen, Thijs Dregmans 
    Last edited: 2023-04-21
--}

module Main where

import System.Environment

main = do [sourcefile, targetfile, codetree] <- getArgs
          putStrLn "done"