module Util (showDelimList, splitOn)
    where

import Data.List

-- Shows every value given, delimited by the given string
showDelimList :: Show a => String -> [a] -> String
showDelimList s l = intercalate s $ map show l

-- Given a list separated by the given value, returns a list of lists with the separator removed
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn sep l =
    let (part, rest) = break (== sep) l
    in if null rest
        then [part]
        else [part] ++ (splitOn sep $ tail rest)
