module Util (showDelimList)
    where

import Data.List

showDelimList :: Show a => String -> [a] -> String
showDelimList s l = intercalate s $ map show l
