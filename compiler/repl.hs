module REPL (repl)
    where

import AST
import Data.Char
import ObjcCodeGen
import Parser
import System.Exit
import System.IO
import System.Process
import Text.Parsec

dropControlCharacters :: String -> (String, String)
dropControlCharacters "" = ("", "")
dropControlCharacters s
    | x == '\EOT' = ("\EOT", "")
    | x == '\n' = ("\n", dropWhile isSpace $ tail s)
    | otherwise =
        let (new_xs, left) = dropControlCharacters $ tail s
        in (x : new_xs, left)
    where
        x = head s

repl' :: String -> [Form] -> IO ()
repl' s formsSoFar =
    let (now, left) = dropControlCharacters s

        fixupBackspaces :: String -> Char -> String
        fixupBackspaces left '\DEL' = init left
        fixupBackspaces left right = left ++ [right]

    in if last now == '\EOT'
        then putStrLn "\nQuit"
        else case (parse Parser.forms "stdin" $ foldl fixupBackspaces "" now) of
             Left err -> do
                 putStrLn $ show err
                 putStr "\n=> "
                 repl' left formsSoFar

             Right forms -> do
                 let newForms = formsSoFar ++ forms
                     objc = codegen newForms

                 putStrLn objc

                 (Just clangIn, _, _, clang) <- createProcess
                     (proc "clang" ["-L", "lib", "-lCocoaClojureRuntime", "-Iruntime/CocoaClojureRuntime", "-xobjective-c", "-Wno-unused-value", "-framework", "Foundation", "-"])
                     { std_in = CreatePipe }

                 hPutStrLn clangIn objc
                 hClose clangIn
                 clangCode <- waitForProcess clang

                 (_, _, _, aout) <- createProcess $ proc "./a.out" []
                 progCode <- waitForProcess aout

                 putStr "\n=> "

                 if (clangCode == ExitSuccess) && (progCode == ExitSuccess)
                    then repl' left newForms
                    else repl' left formsSoFar

repl :: IO ()
repl = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering

    putStr "=> "

    s <- getContents
    repl' s []
