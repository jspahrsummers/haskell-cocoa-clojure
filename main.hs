module Main (main)
    where

import LLVMCodeGen
import Parser
import REPL
import System.Environment
import System.Exit
import System.IO
import Text.Parsec

printErrorAndExit :: ParseError -> IO ()
printErrorAndExit e = do
    putStrLn (show e)
    exitFailure

compile :: [FilePath] -> IO ()
compile (x:xs) = do
    putStrLn $ "*** Compiling " ++ x

    outFD <- openFile (x ++ ".ll") WriteMode
    contents <- readFile x

    either printErrorAndExit (putForms outFD) (parse Parser.forms x contents)

main :: IO ()
main = do
    args <- getArgs
    if null args then repl else compile args
