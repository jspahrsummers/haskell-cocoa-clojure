module IO where

import System.IO

-- Returns the contents of the text file at the given path
getFileContents :: FilePath -> IO String
getFileContents path = do
    fd <- openFile path ReadMode
    str <- hGetContents fd
    return str
