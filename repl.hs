module REPL where

import AST
import Data.Char
import Parser
import System.Exit
import System.IO
import Text.Parsec

dropControlCharacters :: String -> (String, String)
dropControlCharacters "" = ("", "")
dropControlCharacters s
    | x == '\EOT' = ("", "")
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

    in do
        case (parse Parser.forms "stdin" $ foldl fixupBackspaces "" now) of
            Left err -> putStrLn $ show err
            Right forms -> putStrLn $ concatMap show forms

        putStr "=> "

        if null left
            then putStrLn "\nQuit"
            else repl' left

repl :: IO ()
repl = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering

    putStr "=> "

    s <- getContents
    repl' s
