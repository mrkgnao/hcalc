module Calc where

import Data.List (foldl')
import qualified Data.Map as M
import Data.Maybe (fromJust)

-- | Solve an RPN expression.
-- This function takes a string, splits it, and then constructs
-- a stack out of the elements, reducing it as it is constructed.
-- The last remaining element on the stack is the answer.
-- FIXME: Add some kind of error handling!

solveRPN :: String -> Double
solveRPN = head' . foldl' reduceStk [] . words
  where head' [x] = x
        head' xs = error errmsg
          where errmsg =
                  "Error in input, " ++
                  show (length xs) ++
                  " elements remaining on stack instead of 1!"

reduceStk :: [Double] -> String -> [Double]
reduceStk (x:y:ts) op
  | M.member op binOps =
    (lookup' op binOps) x y : ts
reduceStk (x:xs) op
  | M.member op unaryOps =
    (lookup' op unaryOps) x : xs
reduceStk xs op
  | M.member op foldOps =
    [(lookup' op foldOps) xs]
reduceStk xs num = read num : xs

-- | A small convenience function
lookup' :: Ord k => k -> M.Map k v -> v
lookup' k m = fromJust $ M.lookup k m

unaryOps :: M.Map String (Double -> Double)
unaryOps =
  M.fromList
    [("exp",exp),("log",log),("sin",sin),("cos",cos),("tan",tan),("!",fac)]
  where fac = product . enumFromTo 1

binOps :: M.Map String (Double -> Double -> Double)
binOps =
  M.fromList [("+",(+)),("-",(-)),("*",(*)),("/",(/)),("^",(**))]

foldOps :: M.Map String ([Double] -> Double)
foldOps = M.fromList [("sum",sum),("prod",product)]
