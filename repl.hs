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
        let xs = tail s
        in if head xs == '\DEL'
            -- skip over the character before the backspace
            then dropControlCharacters $ tail xs
            else
                let (new_xs, left) = (dropControlCharacters xs)
                in (x : new_xs, left)
    where
        x = head s

repl' :: String -> IO ()
repl' s =
    let (now, left) = dropControlCharacters s
    in do
        case (parse Parser.forms "stdin" now) of
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
