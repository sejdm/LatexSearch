module Main where

import LatexSearch
import System.Environment

main :: IO ()
main = do a <- getArgs
          searchLatex "/home/shane/Dropbox/Algebraic Knots/gluingAll.tex" a
