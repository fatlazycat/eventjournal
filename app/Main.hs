module Main where

import Lib
import JournalFile
import Control.Monad.Trans.State

main :: IO ()
main = do
  initState <- createJournalFile "test.data" 1024
  runStateT (do
    write 'c'
    write 'd'
    sync
            ) initState
  return ()

-- main :: IO ()
-- main = do
--    putStrLn $ "OSX page size = " ++ show sysconfPageSize
--    mmf <- createJournalFile "test.data" 1024
--    write 'a'
--    write 'b'
--    sync
--    return ()
