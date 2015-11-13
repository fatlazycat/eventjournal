module Main where

import Lib
import JournalFile
import System.Posix.Memory
import Foreign.Storable

main :: IO ()
--main = someFunc
main = do
   putStrLn $ "OSX page size = " ++ show sysconfPageSize
   mmf <- createJournalFile "test.data" 1024
   pokeElemOff (currentWriteLocation mmf) 0 'a'
   memorySync (currentWriteLocation mmf) 4096 [MemorySyncSync]
   return ()

-- memorySync :: Ptr a -> CSize -> [MemorySyncFlag] -> IO ()
