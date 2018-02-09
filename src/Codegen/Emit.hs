
import Parser.Syntax
import Codegen.Codegen

codegenTop :: Program -> LLVM ()
codegenTop (Program se) = 

codegen :: SourceElement -> LLVM ()
codegen (SourceElement stmt) = 
codegen (SourceElementFunc (FunctionDeclaration ident param body )) 
  =  define typ ident fnargs bls
    where
      name 
