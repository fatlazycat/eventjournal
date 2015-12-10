module Main where

import           Control.Monad.Trans.State
import qualified Data.ByteString.Char8     as BSC
import           JournalFile

main :: IO ()
main = do
  initState <- createJournalFile "test.data" 1024
  -- evalStateT (do
  s <- execStateT (do
    write (BSC.pack "c")
    write (BSC.pack "d")
    write (BSC.pack "ef")
    write (BSC.pack "gh")
    sync
            ) initState

  res <- evalStateT (sequence [readByteString, readByteString, readByteString, readByteString]) $ reset s
  let resultString = map BSC.unpack res
  mapM_ putStrLn resultString

  return ()
