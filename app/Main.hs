module Main where

import LatexSearch
import System.Environment

main :: IO ()
main = do a <- getArgs
          searchLatexDir "/home/shane/Dropbox/mynotes/" a
