module Calc where

import Data.List (foldl')
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust)
import Control.Monad (liftM2)

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
                  " elements remaining on stack instead of 1!\n" ++
                  "Stack: \n" ++
                  show xs

-- | Push a new element onto the stack and reduce it as far as possible.
reduceStk :: [Double] -> String -> [Double]

-- Binary operators
reduceStk (x:y:ts) op
  | isJust op' = ans : ts
  where op' = M.lookup op binOps
        ans = fromJust op' x y

-- Unary operators
reduceStk (x:xs) op
  | isJust op' = ans : xs
  where op' = M.lookup op unaryOps
        ans = fromJust op' x

-- Operators with arbitrary arity (which "fold" the stack)
reduceStk xs op
  | head op == '_' = map (fromJust $ M.lookup (tail op) unaryOps) xs
  | isJust op' = [ans]
  where op' = M.lookup op foldOps
        ans = fromJust op' xs

-- Predefined constants
reduceStk xs cst
  | isJust cst' = ans : xs
  where cst' = M.lookup cst constants
        ans = fromJust cst'

-- Some number in the input
reduceStk xs num = read num : xs

-- | Unary operators
unaryOps :: M.Map String (Double -> Double)
unaryOps =
  M.fromList
    [("exp",exp),("log",log),("sin",sin),("cos",cos),("tan",tan),("!",fac)]
  where fac = product . enumFromTo 1

-- | Binary operators
binOps :: M.Map String (Double -> Double -> Double)
binOps =
  M.fromList [("+",(+)),("-",(-)),("*",(*)),("/",(/)),("^",(**))]

-- | "Fold" operators
foldOps :: M.Map String ([Double] -> Double)
foldOps =
  M.fromList
    [("sum",sum)
    ,("prod",product)
    ,("max",maximum)
    ,("min",minimum)
    ,("avg",am)
    ,("am",am)
    ,("gm",gm)
    ,("hm",hm)
    ,("qm",rms)
    ,("rms",rms)]

  where powerMean :: Int -> [Double] -> Double
        powerMean p xs =
          (** (1 / p')) . (/ len) . sum $ map (** p') xs
          where p' = fromIntegral p :: Double
                len =
                  fromIntegral $ length xs :: Double

        am, gm, hm, rms :: [Double] -> Double
        am = powerMean 1
        hm = powerMean (-1)
        rms = powerMean 2
        -- GM cannot be represented like this, unless you can write
        -- gm = \x -> lim x 0 (powerMean x), and Haskell can't do that. :P
        -- Now, because I'm fancy:
        gm =
          liftM2 (**) product ((1 /) . fromIntegral . length)

-- | Mathematical (and later other) constants
constants :: M.Map String Double
constants = M.fromList [("pi",pi),("e", exp 1)]
