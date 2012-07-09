module Util (
        delimit,
        maybeDouble,
        maybeFile,
        showDelimited,
        splitOn
    ) where

import qualified Data.Foldable as Foldable
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

-- Joins every string in the given collection with the given delimiter
delimit :: Foldable.Foldable a => String -> a String -> String
delimit s c =
    -- If non-empty...
    if Foldable.any (\_ -> True) c
        then Foldable.foldr1 (\l r -> l ++ s ++ r) c
        else ""

-- Shows every value given, delimited by the given string
-- The given functor must be non-empty
showDelimited :: Foldable.Foldable a => Functor a => Show b => String -> a b -> String
showDelimited s c = delimit s $ fmap show c

-- Given a list separated by the given value, returns a list of lists with the separator removed
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn sep l =
    let (part, rest) = break (== sep) l
    in if null rest
        then [part]
        else part : splitOn sep (tail rest)
