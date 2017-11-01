module Parser where

import Prelude
import Data.List
import Syntax
import Token (Token)
import Token as Token
import Data.Either
import Data.Tuple
import Data.Maybe

parseFunDefList :: List Token
                   -> Either String (Tuple (List FunDef) (List Token))
parseFunDefList xs = p xs Nil
  where
    p :: List Token
         -> List FunDef
         -> Either String (Tuple (List FunDef) (List Token))
    p xs@(Token.Def : _) ys =
      case parseFunDef xs of
        Left e -> Left e
        Right (Tuple funDef restTokens) ->
          p restTokens (funDef : ys)
    p xs ys = Right (Tuple ys xs)


parseFunDef :: List Token
               -> Either String (Tuple FunDef (List Token))
parseFunDef (Token.Def : (Token.Ident id) : Token.LeftPar : xs) =
  case parseParamList xs of
    Left err -> Left err
    Right (Tuple ids (Token.RightPar : Token.LeftBrace : restTokens)) ->
      case parseStmtList restTokens of
        Left e -> Left e
        Right (Tuple stmts (Token.RightBrace : restTokens)) ->
          Right (Tuple (FunDef id ids stmts) restTokens)
        Right _ ->
          Left "expected a closing brace at the end of function body"
    Right _ ->
      Left "missing closing parenthesis after function definition"

parseFunDef _ = Left "not a valid function definition"  
                

foldExprs :: Operator -> List Expr -> Maybe Expr
foldExprs op (x:xs) =
  case foldExprs op xs of
    Nothing -> Just x
    Just rExpr -> Just $ BinExpr x op rExpr
foldExprs op _ = Nothing

parseParamList :: List Token
               -> Either String (Tuple (List String) (List Token))
parseParamList ((Token.Ident id) : xs) = Right (p xs (id : Nil))
  where
    p :: List Token
         -> List String
         -> Tuple (List String) (List Token)
    p (Token.Comma : (Token.Ident id) : xs) ys =
      p xs (id : ys)
    p xs ys = Tuple (reverse ys) xs
      
parseParamList xs = Right (Tuple Nil xs)


s :: List Token -> Either String (Tuple (Maybe Expr) (List Token))
s (Token.Plus:xs) =
  case parseTerm xs of
    Right (Tuple expr tokens) ->
      Right (Tuple (Just expr) tokens)
    Left e -> Left e
s xs = Right (Tuple Nothing xs)

parseList :: forall a. (List Token -> Either String (Tuple (Maybe a) (List Token)))
             -> List Token
             -> List a
             -> Either String (Tuple (List a) (List Token))
parseList f xs exprs =
  case (f xs) of
    Right (Tuple (Just expr) tokens) ->
      parseList f tokens (expr:exprs)
    Right (Tuple Nothing tokens) ->
      Right (Tuple (reverse exprs) tokens)
    Left err ->
      Left err

parseStmt :: List Token -> Either String (Tuple Stmt (List Token))
parseStmt (Token.KwLet : (Token.Ident id) : Token.Assign : xs) =
  case parseExpr xs of
    Right (Tuple expr (Token.Semi : restTokens)) ->
      Right (Tuple (VarDecl id expr) restTokens)
    Right (Tuple expr _) ->
      Left "expected a semicolon after expression"
    Left err -> Left err
    
parseStmt ((Token.Ident id) : Token.Assign : xs) =
  case parseExpr xs of
    Right (Tuple expr (Token.Semi : restTokens)) ->
      Right (Tuple (Assignment id expr) restTokens)
    Right (Tuple expr _) ->
      Left "expected a semicolon"
    Left err -> Left err

parseStmt (Token.Return : xs) =
  case parseExpr xs of
    Left e -> Left e
    Right (Tuple expr (Token.Semi : restTokens)) ->
      Right (Tuple (ReturnStmt expr) restTokens)
    _ ->
      Left "expecting a semicolon after return statement"

parseStmt _ =
  Left "a statement must begin with an identifier or a let kw"


parseStmtList :: List Token -> Either String (Tuple (List Stmt) (List Token))
parseStmtList xs = p xs Nil
  where
    p :: List Token
            -> List Stmt
            -> Either String (Tuple (List Stmt) (List Token))
    p xs ys =
      case xs of
        (Token.Ident _) : _ ->
          case parseStmt xs of
            Right (Tuple stmt restTokens) ->
              p restTokens (stmt : ys)
            Left e -> Left e
        Token.Return : _ ->
          case parseStmt xs of
            Right (Tuple stmt restTokens) ->
              p restTokens (stmt : ys)
            Left e -> Left e
        Token.KwLet : _ ->
          case parseStmt xs of
            Right (Tuple stmt restTokens) ->
              p restTokens (stmt : ys)
            Left e -> Left e
        _ ->
          Right (Tuple (reverse ys) xs)
          



parseExpr :: List Token -> Either String (Tuple Expr (List Token))
parseExpr xs =
  case parseTerm xs of
    Right (Tuple lExpr (Token.Plus : restTokens)) ->
      case parseList s (Token.Plus : restTokens) Nil of
        Left e -> Left e
        Right (Tuple exprs tokens) ->
          case foldExprs OpAdd (lExpr:exprs) of
            Nothing -> Left "unable to build expression"
            Just expr ->
              Right (Tuple expr tokens)

    ret -> ret

parseArgList :: List Token -> Either String (Tuple (List Expr) (List Token))
parseArgList xs =
  case parseExpr xs of
    Right (Tuple expr restTokens) ->
      case parseList parseCommaExpr restTokens Nil of
        Left err -> Left err
        Right (Tuple exprs restTokens) ->
          Right (Tuple (expr : exprs) restTokens)
    Left err -> Left err


parseCommaExpr :: List Token
               -> Either String (Tuple (Maybe Expr) (List Token))
parseCommaExpr (Token.Comma : xs) =
  case parseExpr xs of
    Left err -> Left err
    Right (Tuple expr restTokens) ->
      Right (Tuple (Just expr) restTokens)
parseCommaExpr xs =
  Right $ Tuple Nothing xs



parseTerm :: List Token -> Either String (Tuple Expr (List Token))
parseTerm xs =
  case parseFactor xs of
    Left err -> Left err
    Right (Tuple expr (Token.Mul : tokens)) ->

      case (p (Token.Mul : tokens)) of
        Left err -> Left err
        Right (Tuple (Just rExpr) tokens) ->
          Right (Tuple (BinExpr expr OpMul rExpr) tokens)

        _ -> Left "error"

    Right (Tuple expr tokens) ->
      Right (Tuple expr tokens)
      
  where
    p :: List Token -> Either String (Tuple (Maybe Expr) (List Token))
    p (Token.Mul : xs) =
      case parseFactor xs of
        Left err -> Left err
        Right (Tuple lExpr restTokens) ->
          case p restTokens of
            Left err -> Left err
            Right (Tuple (Just rExpr) restTokens) ->
              Right (Tuple (Just (BinExpr lExpr OpMul rExpr)) restTokens)
            Right (Tuple Nothing restTokens) ->
              Right (Tuple (Just lExpr) restTokens)
              
    p xs =
      Right (Tuple Nothing xs)



parseFactor :: List Token -> Either String (Tuple Expr (List Token))
parseFactor ((Token.Number n) : xs) = Right (Tuple (ConstExpr n) xs)
parseFactor ((Token.Ident id) : Token.LeftPar : Token.RightPar : xs) =
  Right (Tuple (FunCall id Nil) xs)

parseFactor ((Token.Ident id) : Token.LeftPar : xs) =
  case parseArgList xs of
    Right (Tuple exprs (Token.RightPar : restTokens)) ->
      Right (Tuple (FunCall id exprs) restTokens)
    Right (Tuple _ _) ->
      Left "expected a closing parenthesis after parameter list"
    Left err -> Left err
  
parseFactor ((Token.Ident id) : xs) = Right (Tuple (IdentExpr id) xs)
parseFactor (Token.LeftPar : xs) =
  case parseExpr xs of
    Right (Tuple expr (Token.RightPar : restTokens)) ->
      Right (Tuple expr restTokens)
    Right (Tuple _ _) ->
      Left "expecting a closing parenthesis"
    Left err -> Left err

parseFactor _ = Left "unexpected token in parseFactor"



