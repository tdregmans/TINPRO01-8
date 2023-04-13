{--
    sortfile.hs
    TINPRO01-8 (Functional programming 2)
    Sep van der Biezen, Thijs Dregmans 
    Last edited: 2023-04-13
--}

module Main where

import Data.List

import System.Environment

main = do [sourcefile, targetfile] <- getArgs
          filecontent <- readFile sourcefile
          print filecontent
          let sortedFilecontent = sort filecontent
          print sortedFilecontent
          writeFile targetfile sortedFilecontent