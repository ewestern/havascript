{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codegen.Codegen where

import qualified Data.Map as Map
import Data.Word

import Control.Monad.State
import Control.Applicative

import LLVM.General.AST hiding (type')
import LLVM.General.AST.Global
import qualified LLVM.General.AST as AST
import LLVM.General.AST.Type (double)

import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Attribute as A
import qualified LLVM.General.AST.CallingConvention as CC
import qualified LLVM.General.AST.FloatingPointPredicate as FP

newtype LLVM a = LLVM { unLLVM :: State AST.Module a }
  deriving (Functor, Applicative, Monad, MonadState AST.Module )


runLLVM :: AST.Module -> LLVM a -> AST.Module
runLLVM = flip (execState . unLLVM)

emptyModule :: String -> AST.Module
emptyModule label = defaultModule { moduleName = label }

addDefn :: Definition -> LLVM ()
addDefn d = do
  defs <- gets moduleDefinitions
  modify $ \s -> s { moduleDefinitions = defs ++ [d] }

-- MAKE DEFINITIONS
-- function definition
define ::  Type -> String -> [(Type, Name)] -> [BasicBlock] -> LLVM ()
define retty label argtys body = addDefn $
  GlobalDefinition $ functionDefaults {
    name        = Name label
  , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
  , returnType  = retty
  , basicBlocks = body
  }

globals :: Type -> String -> LLVM ()
globals typ label = addDefn $ 
  GlobalDefinition $ globalVariableDefaults {
    name = Name label,
    type' = typ
  }



type SymbolTable = [(String, Operand)]
type Names = Map.Map String Int

data CodegenState
  = CodegenState {
    currentBlock :: Name                     -- Name of the active block to append to
  , blocks       :: Map.Map Name BlockState  -- Blocks for function
  , symtab       :: SymbolTable              -- Function scope symbol table
  , blockCount   :: Int                      -- Count of basic blocks
  , count        :: Word                     -- Count of unnamed instructions
  , names        :: Names                    -- Name Supply
  } deriving Show

data BlockState
  = BlockState {
    idx   :: Int                            -- Block index
  , stack :: [Named Instruction]            -- Stack of instructions
  , term  :: Maybe (Named Terminator)       -- Block terminator
  } deriving Show

newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState )


-- Block
emptyBlock :: Int -> BlockState
emptyBlock i = BlockState i [] Nothing

entry :: Codegen Name
entry = gets currentBlock

uniqueName :: String -> Names -> (String, Names)
uniqueName nm ns =
  case Map.lookup nm ns of
    Nothing -> (nm,  Map.insert nm 1 ns)
    Just ix -> (nm ++ show ix, Map.insert nm (ix+1) ns)

addBlock :: String -> Codegen Name
addBlock bname = do
  bls <- gets blocks
  ix  <- gets blockCount
  nms <- gets names
  let new = emptyBlock ix
      (qname, supply) = uniqueName bname nms

  modify $ \s -> s { blocks = Map.insert (Name qname) new bls
                   , blockCount = ix + 1
                   , names = supply
                   }
  return (Name qname)

setBlock :: Name -> Codegen Name
setBlock bname = do
  modify $ \s -> s { currentBlock = bname }
  return bname

getBlock :: Codegen Name
getBlock = gets currentBlock

modifyBlock :: BlockState -> Codegen ()
modifyBlock new = do
  active <- gets currentBlock
  modify $ \s -> s { blocks = Map.insert active new (blocks s) }

current :: Codegen BlockState
current = do
  c <- gets currentBlock
  blks <- gets blocks
  case Map.lookup c blks of
    Just x -> return x
    Nothing -> error $ "No such block: " ++ show c

fresh :: Codegen Word
fresh = do
  i <- gets count
  modify $ \s -> s { count = 1 + i }
  return $ i + 1

-- Operands
local ::  Type -> Name -> Operand
local typ = LocalReference typ 

assign :: String -> Operand -> Codegen ()
assign var x = do
  lcls <- gets symtab
  modify $ \s -> s { symtab = [(var, x)] ++ lcls }

getvar :: String -> Codegen Operand
getvar var = do
  syms <- gets symtab
  case lookup var syms of
    Just x  -> return x
    Nothing -> error $ "Local variable not in scope: " ++ show var

-- type refers to the type of the lhs of the operand
instr :: Type -> Instruction -> Codegen Operand
instr lhs_typ ins = do -- typ is the return type of the instruction. Or is it?
  n   <- fresh
  blk <- current
  let i = stack blk
  let ref = (UnName n) -- numbered name
  modifyBlock $ blk { stack = i ++ [ref := ins] } -- assign instructin a name and put on stack
  return $ local lhs_typ ref

terminator :: Named Terminator -> Codegen (Named Terminator)
terminator trm = do
  blk <- current
  modifyBlock $ blk { term = Just trm }
  return trm

-- wrapped

-- LHS Type
fadd :: Type -> Operand -> Operand -> Codegen Operand
fadd typ a b = instr typ $ FAdd NoFastMathFlags a b []

fsub :: Type -> Operand -> Operand -> Codegen Operand
fsub typ a b = instr typ $ FSub NoFastMathFlags a b []

fmul :: Type -> Operand -> Operand -> Codegen Operand
fmul typ a b = instr typ $ FMul NoFastMathFlags a b []

fdiv :: Type -> Operand -> Operand -> Codegen Operand
fdiv typ a b = instr typ $ FDiv NoFastMathFlags a b []

{-
cons :: C.Constant -> Operand
cons = ConstantOperand

uitofp :: Type -> Operand -> Codegen Operand
uitofp ty a = instr $ UIToFP a ty []
-}

toArgs :: [Operand] -> [(Operand, [A.ParameterAttribute])]
toArgs = map (\x -> (x, []))

-- control
br :: Name -> Codegen (Named Terminator)
br val = terminator $ Do $ Br val []

cbr :: Operand -> Name -> Name -> Codegen (Named Terminator)
cbr cond tr fl = terminator $ Do $ CondBr cond tr fl []

ret :: Operand -> Codegen (Named Terminator)
ret val = terminator $ Do $ Ret (Just val) []

-- effect
-- LHS Type
call :: Type -> Operand -> [Operand] -> Codegen Operand
call typ fn args = instr typ $ Call Nothing CC.C [] (Right fn) (toArgs args) [] []
-- the return value of the function is bound to the result argument.

alloca :: Type -> Codegen Operand
alloca ty = instr ty $ Alloca ty Nothing 0 []
--- technically, the instruction here returns a pointer type. However, the sample code had it returning a 'double'. Not sure which is correct.

store :: Type -> Operand -> Operand -> Codegen Operand
store typ ptr val = instr typ $ Store False ptr val Nothing 0 []

load :: Type -> Operand -> Codegen Operand
load typ ptr = instr typ $ Load False ptr Nothing 0 []

