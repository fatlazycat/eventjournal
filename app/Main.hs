module Main where

import JournalFile
import Control.Monad.Trans.State

main :: IO ()
main = do
  initState <- createJournalFile "test.data" 1024
  evalStateT (do
    write 'c'
    write 'd'
    sync
            ) initState
  return ()
