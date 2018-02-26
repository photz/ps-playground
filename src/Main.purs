module Main where
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Plus (empty)
import Data.Array (null)
import Data.Array.Partial (tail)
import Data.Either
import Data.Foldable (foldl)
import Data.List (List(..), filter, head, snoc, concatMap, reverse, concat, length)
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
import Syntax
import Token (Token)
import Token as Token
import Tokenizer (tokenize) as Tokenizer
import Parser (parseFunDefList) as Parser

registers :: List Location
registers =
  (Register 1)
  : (Register 2)
  : (Register 3)
  : (Register 4)
  : (Register 5)
  : (Register 6)
  : (Register 7)
  : (Register 8)
  : (Register 9)
  : Nil

backupRegs :: List Location -> List Instruction
backupRegs xs = map (\l -> InstrPush { source : Loc l }) xs

restoreRegs :: List Location -> List Instruction
restoreRegs xs = map (\l -> InstrPop { dest : l }) xs

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
    Just (FunctionEntry { id, params }) ->
      Left $ id <> " is a function"

generateExpr symTab dest (a : locs) (BinExpr lExpr OpAdd rExpr) = do
  lInstr <- (generateExpr symTab a locs lExpr)
  rInstr <- (generateExpr symTab dest locs rExpr)
  last <- Right $ ((InstrAdd { source : Loc a, dest : dest }) : Nil)

  pure (concat (lInstr : rInstr : last : Nil))
   
generateExpr symTab dest _ (ConstExpr n) =
  Right ((InstrMove { source : Constant n, dest : dest }) : Nil)

generateExpr symTab _ _ (FunCall id args) =
  case getLocal symTab id of
    Nothing -> Left $ "no function with name " <> id <> " found"
    Just (VariableEntry { id, loc }) ->
      Left $ id <> " is a variable and cannot be called"
    Just (FunctionEntry { id, params }) ->
      case "main" == id of
        true ->
          Left "The main function cannot be called"
        false ->
          case length args == length params of
            false ->
              Left ("function " <> id <> " expects "
                    <> (show $ length params)
                    <> " arguments, but "
                    <> (show $ length args)
                    <> " were supplied")
            true ->
              case listEither $ List.mapWithIndex (\i arg -> generateExpr symTab (paramToLoc i) registers arg) args of
                Left e -> Left e
                Right instructions ->
                  Right $ concat ((backupRegs registers)
                                  : (concat instructions)
                                  : (Data.List.singleton
                                     (InstrCall id))
                                  : (restoreRegs registers)
                                  : Nil)

generateExpr symTab dest _ _ = Left "not yet implemented"

listEither :: forall a b. List (Either a b) -> Either a (List b)
listEither xs = f xs Nil
  where
    f Nil ys = Right ys
    f ((Right x) : xs) ys = f xs (x : ys)
    f ((Left x) : xs) ys = Left x

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
                 (snoc
                  (snoc instructions
                   (InstrSub { source : Constant (-8)
                             , dest : StackPointer
                             }))
                  (InstrMove { source : Loc head
                             , dest : StackIndex stackIdx }))
                  
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
    Just (FunctionEntry { id, params }) ->
      Left (id <> " is a function and may not occur on the "
            <> " lhs of an assignment")
  
generateStmt symTab (ExprStmt expr) stackIdx =
  case Data.List.uncons registers of
    Nothing -> Left "no available registers"
    Just { head, tail } ->
      case generateExpr symTab head tail expr of
        Left e -> Left e
        Right instructions ->
          Right $ StmtRecord instructions symTab stackIdx


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
                 | InstrSub { source :: Data
                            , dest :: Location
                            }
                 | InstrMul { factor1 :: Data
                            , factor2 :: Data
                            }
                 | InstrRet
                 | InstrCall String


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
  show (InstrCall id) =
    "call"
  show (InstrSub _) = "sub"

instrToGas :: Instruction -> String
instrToGas (InstrMove { source, dest }) =
  "\tmov " <> (dataToGas source) <> ", " <> (locToGas dest)
instrToGas (InstrPush { source }) =
  "\tpush " <> (dataToGas source)
instrToGas (InstrAdd { source, dest }) =
  "\tadd " <> (dataToGas source) <> ", " <> (locToGas dest)
instrToGas (InstrSub { source, dest }) =
  "\tsub " <> (dataToGas source) <> ", " <> (locToGas dest)
instrToGas (InstrMul { factor1, factor2 }) =
  "\tmul " <> (dataToGas factor1) <> ", " <> (dataToGas factor2)
instrToGas InstrRet =
  "\tret"
instrToGas (InstrPop { dest }) =
  "\tpop " <> (locToGas dest)
instrToGas (InstrCall id) =
  "\tcall " <> id

data SymTab = SymTab (M.StrMap SymTabEntry)

data SymTabEntry = VariableEntry { id :: String
                                 , loc :: Location
                                 }
                 | FunctionEntry { id :: String
                                 , params :: List String
                                 }

newSymTab :: SymTab
newSymTab = SymTab (M.insert "log"
            (FunctionEntry { id : "log", params : "value" : Nil })
            M.empty)

hasFunction :: SymTab -> String -> Boolean
hasFunction symTab@(SymTab m) id =
  case M.lookup id m of
    Just (FunctionEntry _) -> true
    _ -> false

-- | Returns a boolean indicating if the program has a main function
hasMain :: SymTab -> Boolean
hasMain (SymTab m) =
  case M.lookup "main" m of
    Just _ -> true
    _ -> false

isMain :: FunDef -> Boolean
isMain (FunDef id _ _) = id == "main"


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
  _ -> SymTab s


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

-- | Creates a global symbol table with an entry for each function
globalSymTab :: List FunDef -> SymTab
globalSymTab xs = foldl ins newSymTab xs
  where
    ins :: SymTab -> FunDef -> SymTab
    ins (SymTab symTab) (FunDef id params _) =
      SymTab (M.insert id (FunctionEntry { id, params }) symTab)

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

def f1(a, b) {
  log(a + b);
}

def main () {
  let a = 2 + 3 + 4;
  let b = 10;
  f1(a, b);
}

"""

gen :: SymTab -> FunDef -> Either String String
gen symTab x@(FunDef id params _) = do
  instructions <- generateFun symTab x
  pure (""
        <> "\t.type " <> id <> ", @function\n"
        <> "\t.globl " <> id <> "\n"
        <> id <> ":\n"
        <> (((foldl (\a b -> a <> "\n" <> b) "") <<< (map instrToGas)) instructions))

        

generateProg :: List FunDef -> Either String String
generateProg xs =
  case hasMain symTab of
    false -> Left "a main function must be defined"
    true -> do
      funcDefs <- g (map (gen symTab) xs) Nil
      pure ("\t.file \"nothing\"\n"
        <> "\t.text\n"
        <> ".LC0:\n"
        <> "\t.string \"%ld\\n\"\n"
        <> "log:"
        <> "\tpush %rbp\n"
        <> "\tmov %rsp, %rbp\n"
        <> "\tmov %rdi, %rsi\n"
        <> "\tmovl $.LC0, %edi\n"
        <> "\tmovl $0, %eax\n"
        <> "\tcall printf\n"
        <> "\tpopq %rbp\n"
        <> "\tret\n"
        <> (foldl (\a b -> a <> "\n" <> b) "" funcDefs))

  where
    g :: List (Either String String)
         -> List String
         -> Either String (List String)
    g Nil ys = Right ys
    g ((Right x) : xs) ys = g xs (x : ys)
    g ((Left x) : _) _ = Left x

    symTab :: SymTab
    symTab = globalSymTab xs


evalExpr :: Expr -> Int
evalExpr (ConstExpr n) = n
evalExpr (BinExpr l OpMul r) = (evalExpr l) * (evalExpr r)
evalExpr (BinExpr l OpAdd r) = (evalExpr l) + (evalExpr r)
evalExpr (BinExpr l OpSub r) = (evalExpr l) + (evalExpr r)
evalExpr _ = 0

build :: String -> Either String String
build source = do
  tokens <- Right $ Tokenizer.tokenize source
  Tuple funDefs _ <- Parser.parseFunDefList tokens
  prog <- generateProg funDefs
  pure prog

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log $ case build test of
    Left e ->
      "Error: " <> e
    Right prog ->
      prog
