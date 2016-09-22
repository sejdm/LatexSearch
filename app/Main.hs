module Main where

import LatexSearch
import System.Environment

main :: IO ()
main = do a <- getArgs
          searchLatex a
