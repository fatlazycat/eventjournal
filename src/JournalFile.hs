module JournalFile (
  createJournalFile
                   ) where

import System.IO

createJournalFile :: FilePath -> Integer -> IO()
createJournalFile fp size = do
    h <- openBinaryFile fp ReadWriteMode
    hSetFileSize h size
    hClose h
