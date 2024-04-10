module Main where

import Grammar
import Lexer (alexScanTokens)
import Parser

import Data.List (sort)
import Data.Map as Map
import Data.Map (insert)
import Data.Maybe

checkHyp :: [Expr] -> Expr -> Result
checkHyp context expr = let hyps = Map.fromList(zip context [1..])
                          in case Map.findWithDefault 0 expr hyps of
                            0 -> Incorrect
                            i -> Hypothesis i

checkAx :: Expr -> Result
checkAx expr@(Binary Impl a (Binary Impl b a2))
      | a == a2                                    = Axiom 1
checkAx expr@(Binary Impl (Binary Impl a b) (Binary Impl (Binary Impl a2 (Binary Impl b2 c)) (Binary Impl a3 c2)))
      | a == a2 && a2 == a3 && b == b2 && c == c2  = Axiom 2
checkAx expr@(Binary Impl a (Binary Impl b (Binary And a2 b2)))
      | a == a2 && b == b2                         = Axiom 3
checkAx expr@(Binary Impl (Binary And a b) a2)
      | a == a2                                    = Axiom 4
checkAx expr@(Binary Impl (Binary And a b) b2)
      | b == b2                                    = Axiom 5
checkAx expr@(Binary Impl a (Binary Or a2 b))
      | a == a2                                    = Axiom 6
checkAx expr@(Binary Impl b (Binary Or a b2))
      | b == b2                                    = Axiom 7
checkAx expr@(Binary Impl (Binary Impl a c) (Binary Impl (Binary Impl b c2) (Binary Impl (Binary Or a2 b2) c3)))
      | a == a2 && b == b2 && c == c2 && c2 == c3  = Axiom 8
checkAx expr@(Binary Impl (Binary Impl a b) (Binary Impl (Binary Impl a2 (Not b2)) (Not a3)))
      | a == a2 && a2 == a3 && b == b2             = Axiom 9
checkAx expr@(Binary Impl (Not (Not a)) a2)
      | a == a2                                    = Axiom 10
checkAx _                                          = Incorrect

checkMP :: [Expr] -> Expr -> Map.Map [Expr] (Map.Map Expr (Map.Map Expr Int)) -> Map.Map [Expr] (Map.Map Expr Int) -> Result
checkMP context expr right left = let
                                    sameContext = Map.findWithDefault Map.empty context right
                                    sameRight   = Map.findWithDefault Map.empty expr sameContext
                                    sameLeft    = Map.findWithDefault Map.empty context left
                                      in case Map.intersection sameRight sameLeft of
                                        common | not (Map.null common) -> let key = head $ keys common
                                                    in ModusPonens (sameLeft ! key) (sameRight ! key)
                                        _ -> Incorrect

deductor :: [Expr] -> Expr -> Line
deductor context expr@(Binary Impl a b) = deductor (a : context) b
deductor context expr = Line context expr

checkDed :: [Expr] -> Expr -> Map.Map [Expr] (Map.Map Expr Int) -> Result 
checkDed context expr deduction = let
                                    sameContext = Map.findWithDefault Map.empty context deduction
                                      in case Map.findWithDefault 0 expr sameContext of
                                        0 -> Incorrect
                                        i -> Deduction i

addImpl :: [Expr] -> Expr -> Expr -> Int
        -> Map.Map [Expr] (Map.Map Expr (Map.Map Expr Int))
        -> Map.Map [Expr] (Map.Map Expr (Map.Map Expr Int))
addImpl c a b n m = case Map.lookup c m of
    (Nothing) -> Map.insert c (Map.singleton b (Map.singleton a n)) m
    (Just m2) -> case Map.lookup b m2 of
        (Nothing) -> Map.insert c (Map.insert b (Map.singleton a n) m2) m
        (Just m3) -> case Map.lookup a m3 of
            (Nothing) -> Map.insert c (Map.insert b (Map.insert a n m3) m2) m
            _         -> m

addLine :: [Expr] -> Expr -> Int
        -> Map.Map [Expr] (Map.Map Expr Int)
        -> Map.Map [Expr] (Map.Map Expr Int)
addLine c e n m = case Map.lookup c m of
    (Nothing) -> Map.insert c (Map.singleton e n) m
    (Just m2) -> case Map.lookup e m2 of
        (Nothing) -> Map.insert c (Map.insert e n m2) m
        _         -> m

process :: [String] -> Int -> Map.Map [Expr] (Map.Map Expr (Map.Map Expr Int)) -> Map.Map [Expr] (Map.Map Expr Int) -> Map.Map [Expr] (Map.Map Expr Int) -> IO()
process [] _ _ _ _ = return()
process (x:xs) num right left deduction = do
  let
    (Line context expr) = parseLine $ alexScanTokens x
    sorted = (sort . reverse) context
    (Line rawDed dedExpr) = deductor context expr
    dedCont = (sort . reverse) rawDed
    
    res = case checkHyp (reverse context) expr of
      (Incorrect) -> case checkAx expr of
        (Incorrect) -> case checkMP sorted expr right left of
          (Incorrect) -> checkDed dedCont dedExpr deduction
          mp -> mp
        ax -> ax
      hyp -> hyp
  
  putStrLn ("[" ++ (show num) ++ "] " ++ (show (Line (reverse context) expr)) ++ (show res))
  let
    right2 = case expr of
      (Binary Impl a b) -> addImpl sorted a b num right
      _ -> right
    left2 = addLine sorted expr num left
    deduction2 = addLine dedCont dedExpr num deduction
      in process xs (num + 1) right2 left2 deduction2

main :: IO ()
main = do
  input <- getContents
  process (lines input) 1 Map.empty Map.empty Map.empty