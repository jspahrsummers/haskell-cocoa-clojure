module LLVMCodeGen where

import AST
import LLVM.Core
import System.IO

putForms :: Handle -> [Form] -> IO ()
putForms fd forms = putStrLn "not implemented"
