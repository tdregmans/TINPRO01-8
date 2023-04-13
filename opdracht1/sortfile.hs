module Main where

import Data.List

import System.Environment

main = do [sourcefile, targetfile]<- getArgs
          filecontent <- readFile sourcefile
          let sortedFilecontent = sort filecontent
          print sortedFilecontent
          writeFile targetfile sortedFilecontent