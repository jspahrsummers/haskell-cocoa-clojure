module Main where

import LLVMCodeGen
import Parser
import System.Exit
import System.IO
import Text.Parsec

printErrorAndExit :: ParseError -> IO ()
printErrorAndExit e = do
    putStrLn (show e)
    exitFailure

main :: IO ()
main = do
    outFD <- openFile "output.ll" WriteMode
    contents <- readFile "input.clj"
    either printErrorAndExit (putForms outFD) (parse Parser.forms "" contents)
