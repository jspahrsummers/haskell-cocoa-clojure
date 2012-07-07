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

repl' :: String -> IO ()
repl' s =
    let (now, left) = dropControlCharacters s

        fixupBackspaces :: String -> Char -> String
        fixupBackspaces left '\DEL' = init left
        fixupBackspaces left right = left ++ [right]

    in if last now == '\EOT'
        then putStrLn "\nQuit"
        else do
            case (parse Parser.forms "stdin" $ foldl fixupBackspaces "" now) of
                Left err -> putStrLn $ show err
                Right forms -> do
                    putStrLn $ concatMap show forms

                    objc <- codegen forms
                    putStrLn objc

                    (Just clangIn, _, _, clang) <- createProcess
                        (proc "clang" ["-xobjective-c", "-Wno-unused-value", "-framework", "Foundation", "-"])
                        { std_in = CreatePipe }

                    hPutStrLn clangIn objc
                    hClose clangIn
                    waitForProcess clang

                    (_, _, _, aout) <- createProcess $ proc "./a.out" []
                    waitForProcess aout

                    return ()

            putStr "=> "
            repl' left

repl :: IO ()
repl = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering

    putStr "=> "

    s <- getContents
    repl' s
