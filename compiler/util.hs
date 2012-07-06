module Util (showDelimList)
    where

showDelimList :: Show a => String -> [a] -> String
showDelimList s [] = ""
showDelimList s (x:xs) =
    let sepThenShow x = s ++ (show x)
    in (show x) ++ (concatMap sepThenShow xs)
