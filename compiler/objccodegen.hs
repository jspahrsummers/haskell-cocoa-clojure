module ObjcCodeGen (codegen, codegenToFile)
    where

import qualified AST as A
import Control.Monad.State.Lazy
import Control.Monad.Writer.Lazy
import System.IO
import Util

codegen :: [A.Form] -> IO String
codegen forms = return "foo"

codegenToFile :: Handle -> [A.Form] -> IO ()
codegenToFile fd forms = do
    str <- codegen forms
    hPutStrLn fd str
