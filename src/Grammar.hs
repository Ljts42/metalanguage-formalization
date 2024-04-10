module Grammar where

import Data.List (intercalate)

data Binop = Impl | Or | And deriving (Eq, Ord)

instance Show Binop where
  show Impl = "->"
  show Or   = "|"
  show And  = "&"

data Expr = Binary Binop Expr Expr
          | Not Expr
          | Var String
          deriving (Eq, Ord)

instance Show Expr where
  show (Binary op a b) = "(" ++ show a ++ show op ++ show b ++ ")"
  show (Not e)         = "!" ++ show e
  show (Var name)      = name

data Line = Line [Expr] Expr

instance Show Line where
  show (Line c e)  = intercalate "|-" [intercalate "," $ map show c, show e]

data Result = Hypothesis Int
            | Axiom Int
            | ModusPonens Int Int
            | Deduction Int
            | Incorrect

instance Show Result where
  show (Hypothesis i)     = " [Hyp. " ++ show i ++ "]"
  show (Axiom i)          = " [Ax. sch. " ++ show i ++ "]"
  show (ModusPonens i j)  = " [M.P. " ++ show i ++ ", " ++ show j ++ "]"
  show (Deduction i)      = " [Ded. " ++ show i ++ "]"
  show (Incorrect)        = " [Incorrect]"