module Main where
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Plus (empty)
import Data.Array (null)
import Data.Array.Partial (tail)
import Data.Either
import Data.Foldable (foldl)
import Data.List (List(..), filter, head, snoc, concatMap, reverse, concat)
import Data.List as Data.List
import Data.List.Types (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Show (show)
import Data.String (uncons, takeWhile, dropWhile, drop, joinWith)
import Data.Tuple
import Debug.Trace (traceAnyA, spy, traceA, traceAnyM, traceAny, traceShow, trace)
import Math (sqrt, pi)
import Partial.Unsafe (unsafePartial)
import Prelude
import Data.StrMap as M
import Data.Traversable (sequence)
import Data.List as List

import Token (Token)
import Token as Token
import Tokenizer (tokenize)

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
      Right (Tuple exprs tokens)
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



evalExpr :: Expr -> Int
evalExpr (ConstExpr n) = n
evalExpr (BinExpr l OpMul r) = (evalExpr l) * (evalExpr r)
evalExpr (BinExpr l OpAdd r) = (evalExpr l) + (evalExpr r)
evalExpr (BinExpr l OpSub r) = (evalExpr l) + (evalExpr r)
evalExpr _ = 0

registers :: List Location
registers =
  (Register 1) : (Register 2) : (Register 3) : (Register 4) : Nil

data Location = StackIndex StackIdx
              | StackPointer
              | BasePointer
              | Register Int

locToGas :: Location -> String
locToGas (StackIndex (StackIdx i)) = (show i) <> "(%rbp)"
locToGas (Register r) =
  case r of
    0 -> "%rbx"
    1 -> "%rcx"
    2 -> "%rdx"
    3 -> "%r8"
    4 -> "%r9"
    5 -> "%r10"
    6 -> "%r11"
    7 -> "%r12"
    8 -> "%r13"
    9 -> "%r14"
    10 -> "%r15"
    11 -> "%rdi"
    12 -> "%rsi"
    _ -> "NO SUCH REGISTER"
locToGas BasePointer = "%rbp"
locToGas StackPointer = "%rsp"

instance showLocation :: Show Location where
  show (StackIndex i) = "StackIndex " <> (show i)
  show (Register r) = "Register " <> (show r)
  show StackPointer = "StackPointer"
  show BasePointer = "BasePointer"

data Data = Constant Int
          | Loc Location

dataToGas :: Data -> String
dataToGas (Constant n) = "$" <> (show n)
dataToGas (Loc loc) = locToGas loc

instance showData :: Show Data where
  show (Constant n) = "Constant " <> (show n)
  show (Loc loc) = "Location " <> (show loc)


generateExpr :: SymTab
             -> Location
             -> List Location
             -> Expr
             -> Either String (List Instruction)
generateExpr _ dest _ (ConstExpr n) =
  Right ((InstrMove { source : (Constant n), dest : dest }) : Nil)

generateExpr symTab dest locs (IdentExpr id) =
  case getLocal symTab id of
    Nothing -> Left ("identifier " <> id <> " undeclared in scope")
    Just (VariableEntry { id, loc }) ->
      Right ((InstrMove { source : Loc loc, dest : dest }) : Nil)

generateExpr symTab dest (a : locs) (BinExpr lExpr OpAdd rExpr) = do
  lInstr <- (generateExpr symTab a locs lExpr)
  rInstr <- (generateExpr symTab dest locs rExpr)
  last <- Right $ ((InstrAdd { source : Loc a, dest : dest }) : Nil)

  pure (concat (lInstr : rInstr : last : Nil))
   
generateExpr symTab dest _ (ConstExpr n) =
  Right ((InstrMove { source : Constant n, dest : dest }) : Nil)


generateExpr symTab dest _ (FunCall id args) = Left "not yet implemented"

generateExpr symTab dest _ _ = Left "not yet implemented"

data StmtRecord = StmtRecord (List Instruction) SymTab StackIdx

generateStmt :: SymTab
             -> Stmt
             -> StackIdx
             -> Either String StmtRecord
generateStmt symTab (VarDecl id expr) stackIdx | not $ hasLocal symTab id =
  case Data.List.uncons registers of
    Nothing -> Left "no registers available to store temp results"
    Just { head, tail } ->
      case generateExpr symTab head tail expr of
        Left e -> Left e
        Right instructions ->
          Right (StmtRecord
                 (snoc instructions
                  (InstrMove { source : Loc head, dest : StackIndex stackIdx }))
                  
                 (insertLocal symTab
                  (VariableEntry { id : id, loc : StackIndex stackIdx }))
                 nextStack)
  where nextStack = incStackIdx stackIdx

generateStmt _ (VarDecl id _) stackIdx =
  Left (id <> " has already been defined")

generateStmt symTab (Assignment id expr) stackIdx =
  case getLocal symTab id of
    Nothing -> Left $ "missing declaration for " <> id
    Just (VariableEntry { id, loc }) ->
      case generateExpr symTab loc registers expr of
        Left e -> Left e
        Right instructions ->
          Right (StmtRecord instructions symTab stackIdx)
  

generateStmt symTab (ReturnStmt expr) stackIdx =
  Right (StmtRecord (InstrRet : Nil) symTab stackIdx)

data Instruction = InstrMove { source :: Data
                             , dest :: Location
                             }
                 | InstrPush { source :: Data }
                 | InstrPop { dest :: Location }
                 | InstrAdd { source :: Data
                            , dest :: Location
                            }
                 | InstrMul { factor1 :: Data
                            , factor2 :: Data
                            }
                 | InstrRet


instance showInstruction :: Show Instruction where
  show (InstrMove { source, dest }) =
    "move " <> (show source) <> " " <> (show dest)
  show (InstrPush { source }) =
    "push " <> (show source)
  show (InstrAdd { source, dest }) =
    "add " <> (show source) <> " " <> (show dest)
  show (InstrMul { factor1, factor2 }) =
    "mul " <> (show factor1) <> " " <> (show factor2)
  show InstrRet =
    "ret"
  show (InstrPop { dest }) =
    "pop"

instrToGas :: Instruction -> String
instrToGas (InstrMove { source, dest }) =
  "\tmov " <> (dataToGas source) <> ", " <> (locToGas dest)
instrToGas (InstrPush { source }) =
  "\tpush " <> (dataToGas source)
instrToGas (InstrAdd { source, dest }) =
  "\tadd " <> (dataToGas source) <> ", " <> (locToGas dest)
instrToGas (InstrMul { factor1, factor2 }) =
  "\tmul " <> (dataToGas factor1) <> ", " <> (dataToGas factor2)
instrToGas InstrRet =
  "\tret"
instrToGas (InstrPop { dest }) =
  "\tpop " <> (locToGas dest)

data SymTab = SymTab (M.StrMap SymTabEntry)

data SymTabEntry = VariableEntry { id :: String, loc :: Location }

newSymTab :: SymTab
newSymTab = SymTab M.empty

hasLocal :: SymTab -> String -> Boolean
hasLocal (SymTab s) id = M.member id s

getLocal :: SymTab -> String -> Maybe SymTabEntry
getLocal (SymTab s) id = M.lookup id s

insertLocal :: SymTab
            -> SymTabEntry
            -> SymTab
insertLocal (SymTab s) entry = case entry of
  VariableEntry { id, loc } ->
    SymTab (M.insert id entry s)



data StackIdx = StackIdx Int
instance showStackIdx :: Show StackIdx where
  show (StackIdx i) = "StackIdx" <> (show i)

newStack :: StackIdx
newStack = StackIdx (-8)

incStackIdx :: StackIdx -> StackIdx
incStackIdx (StackIdx i) = StackIdx (i - 8)


g :: (SymTab
      -> Stmt
      -> StackIdx
      -> Either String StmtRecord
     )
     -> SymTab
     -> List Stmt
     -> Either String (List Instruction)
g f = g2 newStack Nil
  where
    g2 stackIdx instructions symTab (stmt : stmts) = 
      case (f symTab stmt stackIdx) of
        Left e -> Left e
        Right (StmtRecord newInstructions newSymTab newStackIdx) ->
          g2 newStackIdx (concat (instructions : newInstructions : Nil)) newSymTab stmts
    g2 stackIdx instructions _ Nil =
      Right instructions
    

paramToLoc :: Int -> Location
paramToLoc 0 = Register 11  -- rdi
paramToLoc 1 = Register 12 -- "rsi"
paramToLoc 2 = Register 2 -- "rdx"
paramToLoc 3 = Register 1 -- "rcx"
paramToLoc 4 = Register 3 -- "r8"
paramToLoc 5 = Register 9 --  "r9"
paramToLoc _ = Register 9999999 -- use turn this into a maybe

addParams :: List String -> SymTab -> SymTab
addParams params symTab =
  foldl (\symTab (Tuple i paramId) ->
          insertLocal symTab
          (VariableEntry { id : paramId
                         , loc : paramToLoc i
                         }))
  symTab
  (List.mapWithIndex (\i a -> Tuple i a) params)

generateFun :: SymTab
            -> FunDef
            -> Either String (List Instruction)
generateFun symTab (FunDef id params stmts) = do
  instructions <- g generateStmt (addParams params symTab) stmts
  pure ((InstrPush { source : Loc BasePointer })
        : (InstrMove { source : Loc StackPointer
                     , dest :  BasePointer
                     })
        : (snoc
           instructions
           (InstrPop { dest : BasePointer })))

test :: String
test = """

def f1 (a) {
a = 0;
}

def main () {
let x = (1 + 1 + 1) + 1 + (1);
let y = x + 10;
let z = y + 10;
let w = f1(1);
}

"""

gen :: FunDef -> Either String String
gen x@(FunDef id params _) = do
  instructions <- generateFun newSymTab x
  pure ("\t.globl " <> id <> "\n"
        <> "\t.type " <> id <> ", @function\n"
        <> id <> ":\n"
        <> (((foldl (\a b -> a <> "\n" <> b) "") <<< (map instrToGas)) instructions))

        

genProg :: List FunDef -> Either String String
genProg xs = do
  funcDefs <- g (map gen xs) Nil
  pure ("\t.file \"nothing\"\n"
        <> "\t.text\n"
        <> (foldl (\a b -> a <> "\n" <> b) "" funcDefs))

  where
    g :: List (Either String String)
         -> List String
         -> Either String (List String)
    g Nil ys = Right ys
    g ((Right x) : xs) ys = g xs (x : ys)
    g ((Left x) : _) _ = Left x


main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log (case parseFunDefList tokensList of
          Left e ->
            "error while parsing: " <> e <> "\n" <> (show tokensList)
          Right (Tuple res _) ->

            (show res)

            <> case genProg res of
              Left e -> e
              Right p -> p)
    where
      tokensList = tokenize test
      funDefList = parseFunDefList tokensList



