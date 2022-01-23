module Main where

import System.Environment
import Compiler

--TODO Task 3.4
main :: IO ()
main = do args <- getArgs
          let exec = read(concat args)
          print(ccomp exec)