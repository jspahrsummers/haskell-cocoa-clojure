module Util (
        maybeDouble, maybeFile, showDelimList, splitOn
    ) where

import Data.List
import System.Directory
import System.IO

-- Determines whether a given file path exists, returning Nothing if it does not
maybeFile :: FilePath -> IO (Maybe FilePath)
maybeFile p = do
    e <- doesFileExist p
    return $ if e then Just p else Nothing

-- Converts a Rational into a Double, only if it would not lose precision
maybeDouble :: Rational -> Maybe Double
maybeDouble r = 
    let dbl = fromRational r :: Double
    in if toRational dbl == r then Just dbl else Nothing

-- Shows every value given, delimited by the given string
showDelimList :: Show a => String -> [a] -> String
showDelimList s l = intercalate s $ map show l

-- Given a list separated by the given value, returns a list of lists with the separator removed
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn sep l =
    let (part, rest) = break (== sep) l
    in if null rest
        then [part]
        else part : splitOn sep (tail rest)
