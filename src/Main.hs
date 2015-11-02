module Main where

import Calc

main :: IO ()
main =
  putStr "Enter expression: " >> getLine >>=
  (\str ->
     case solveRPN str of
       Left err -> putStrLn err
       Right val -> print val) >>
  putStr "Do you want to go again? (y/n) " >>
  getLine >>=
  \x ->
    case x of
      "y" -> putStrLn "" >> main
      _ -> return ()
