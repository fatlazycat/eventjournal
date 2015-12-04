module Main where

import JournalFile
import Control.Monad.Trans.State
import qualified Data.ByteString.Char8 as BSC

main :: IO ()
main = do
  initState <- createJournalFile "test.data" 1024
  evalStateT (do
    write (BSC.pack "c")
    write (BSC.pack "d")
    write (BSC.pack "ef")
    write (BSC.pack "gh")
    sync
            ) initState
  return ()
