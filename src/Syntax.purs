module Syntax where
import Prelude
import Data.List

data FunDef = FunDef String (List String) (List Stmt)

instance showFunDef :: Show FunDef where
  show (FunDef id params body) =
    "Function " <> (show id) <> "(" <> (show params) <> ") {" <> (show body)


data Operator = OpAdd | OpSub | OpMul | OpDiv

data Expr = BinExpr Expr Operator Expr
          | ConstExpr Int
          | IdentExpr String
          | FunCall String (List Expr)

instance showExpr :: Show Expr where
  show (ConstExpr n) = "ConstExpr " <> (show n)
  show (IdentExpr id) = "IdentExpr "<> id
  show (BinExpr l op r) = "BinExpr " <> (show l) <> " " <> (show r)
  show (FunCall id params) = "FunCall " <> id

data Stmt = Assignment String Expr
          | VarDecl String Expr
          | ReturnStmt Expr

instance showStmt :: Show Stmt where
  show (Assignment id expr) = "Assignment " <> id <> " " <> (show expr)
  show (VarDecl id expr) = "Variable decl " <> id <> " " <> (show expr)
  show (ReturnStmt expr) = "Return " <> (show expr)
