module Tokenizer (tokenize) where

import Data.Char (toCharCode)
import Data.Int (fromString)
import Prelude
import Control.Plus (empty)
import Data.String (uncons, takeWhile, dropWhile, drop, joinWith)
import Data.Tuple
import Data.List (List(..), filter, head, snoc, concatMap, reverse, concat)
import Data.List as Data.List
import Data.List.Types (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Show (show)
import Data.List as List
import Token (Token)
import Token as Token
  
isLetter :: Char -> Boolean
isLetter c =
  (97 <= code && code <= 122) || (65 <= code && code <= 90)
  where
    code = toCharCode c

isDigit :: Char -> Boolean
isDigit c =
  (48 <= code && code <= 57)
  where
    code = toCharCode c

isLetterOrDigit :: Char -> Boolean
isLetterOrDigit c = (isDigit c) || (isLetter c)

tok :: Char -> Maybe Token
tok '(' = Just Token.LeftPar
tok ')' = Just Token.RightPar
tok '{' = Just Token.LeftBrace
tok '}' = Just Token.RightBrace
tok '+' = Just Token.Plus
tok '-' = Just Token.Minus
tok '*' = Just Token.Mul
tok '/' = Just Token.Div
tok '=' = Just Token.Assign
tok ';' = Just Token.Semi
tok ',' = Just Token.Comma 
tok _ = Nothing

tokenize :: String -> List Token
tokenize "" = empty
tokenize xs =
  case uncons xs of
    Nothing -> empty
    Just { head, tail } ->
      case tok head of
        Just token -> token : (tokenize tail)
        Nothing ->
          case head of
            '\n' -> tokenize tail
            ' ' -> tokenize tail
            '#' ->
              tokenize $ dropWhile (\x -> not (x == '\n')) tail
            _ | isLetter head ->
              Cons newToken (tokenize (dropWhile isLetterOrDigit xs))
            _ | isDigit head ->
              case fromString $ takeWhile isDigit xs of
                Nothing -> empty
                Just n ->
                  Cons (Token.Number n) (tokenize (dropWhile isDigit xs))
            _ -> empty
          where 
            newToken =
              case takeWhile isLetterOrDigit xs of
                "let" -> Token.KwLet
                "def" -> Token.Def
                "return" -> Token.Return
                x -> Token.Ident x
