module Token where

import Prelude
import Data.Show (show)

data Token = Assign
           | Comma
           | Def
           | Div
           | Ident String
           | KwLet
           | LeftBrace
           | LeftPar
           | Minus
           | Mul
           | Number Int
           | Plus
           | RightBrace
           | RightPar
           | Semi
           | Return


instance showToken :: Show Token where
  show (Ident s) = "Ident " <> s
  show (Number n) = "Number " <> (show n)
  show Assign = "="
  show Comma = "Comma"
  show Def = "Def"
  show Div = "div"
  show KwLet = "let"
  show LeftBrace = "LeftBrace"
  show LeftPar = "LeftPar"
  show Minus = "-"
  show Mul = "mul"
  show Plus = "+"
  show RightBrace = "RightBrace"
  show RightPar = "RightPar"
  show Semi = "Semi"
  show Return = "Return"
