module Main where

import Calc

main :: IO ()
main =
  putStr "Enter expression: " >> getLine >>= print . solveRPN >>
  putStr "Do you want to go again? (y/n) " >>
  getLine >>=
  \x ->
    case x of
      "y" -> putStrLn "" >> main
      _ -> return ()
