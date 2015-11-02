module Main where

import Calc

main :: IO ()
main = do
  putStr "Enter expression: "
  expr <- getLine
  print (solveRPN expr)
