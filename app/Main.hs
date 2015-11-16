module Main where

import Lib
import JournalFile
import System.Posix.Memory
import Foreign.Storable
import Foreign.Ptr

main :: IO ()
--main = someFunc
main = do
   putStrLn $ "OSX page size = " ++ show sysconfPageSize
   mmf <- createJournalFile "test.data" 1024
   pokeElemOff (currentWriteLocation mmf) 0 'a'
   pokeElemOff (plusPtr (currentWriteLocation mmf) (sizeOf 'a')) 0 'b'
   memorySync (currentWriteLocation mmf) 4096 [MemorySyncSync]
   return ()

-- memorySync :: Ptr a -> CSize -> [MemorySyncFlag] -> IO ()
