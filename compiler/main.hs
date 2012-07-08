module Main (main)
    where

import AST
import Control.Applicative
import Data.Char
import ObjcCodeGen
import Parser
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Process
import Text.Parsec (parse, ParseError)
import Util

printErrorAndExit :: ParseError -> IO ()
printErrorAndExit e = do
    hPutStrLn stderr $ show e
    exitFailure

main :: IO ()
main = do
    args <- getArgs
    if null args then repl else compile args

{-
    Compilation
-}
compile :: [FilePath] -> IO ()
compile (x:xs) = do
    putStrLn $ "*** Compiling " ++ x

    outFD <- openFile (replaceExtension x "m") WriteMode
    contents <- readFile x

    either printErrorAndExit (codegenToFile outFD) (parse Parser.forms x contents)

{-
    REPL
-}
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
                 hPutStrLn stderr $ show err
                 putStr "\n=> "
                 repl' left formsSoFar

             Right forms -> do
                 expanded <- expandRequires forms

                 let newForms = formsSoFar ++ expanded
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

{-
    Imports
-}

-- Looks for a file matching the given name in the library search paths
libInPaths :: String -> IO (Maybe FilePath)
libInPaths name =
    let nameParts = splitOn '.' name

        pathInDir :: FilePath -> IO (Maybe FilePath)
        pathInDir d = maybeFile $ foldl (</>) d nameParts <.> "cljm"

    -- TODO: allow entries here to be provided on the command line
    -- TODO: include the directory of the original source file
    in liftA2 (<|>) (pathInDir "") (pathInDir "lib")

-- Collects the ASTs for the named libraries
importLibs :: [String] -> IO [Form]
importLibs [] = return []
importLibs (x:xs) = do
    mp <- libInPaths x

    case mp of
        (Just p) -> do
            contents <- readFile p

            let ef = parse Parser.forms p contents
            case ef of
                (Left err) -> [] <$ printErrorAndExit err
                (Right forms) -> do
                    rest <- importLibs xs
                    return $ forms ++ rest

        Nothing -> do
            hPutStrLn stderr $ "*** Could not find library " ++ x
            exitFailure
            return []

-- Expands (require ...) into the parsed AST of the imported file
-- TODO: this should use some minimal set of declarations from the file instead of all its definitions
-- TODO: expand nested requires
expandRequires :: [Form] -> IO [Form]
expandRequires [] = return []
expandRequires ((List ((Symbol "require"):libs)):xs) = do
    expanded <- importLibs $ map (\(Symbol l) -> l) libs
    rest <- expandRequires xs
    return $ expanded ++ rest

expandRequires (x:xs) = do
    rest <- expandRequires xs
    return $ x : rest
