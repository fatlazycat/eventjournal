module Main where

import Lib
import JournalFile
import System.Posix.Memory

main :: IO ()
--main = someFunc
main = do
   putStrLn $ "OSX page size = " ++ show sysconfPageSize
   createJournalFile "test.data" 1024
