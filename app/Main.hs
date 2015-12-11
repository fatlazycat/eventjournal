module Main where

import           Control.Monad.Trans.State
import qualified Data.ByteString.Char8     as BSC
import           JournalFile

main :: IO ()
main = do
  initState <- createJournalFile "test.data" 1024

  s <- execStateT (
    sequence [write (BSC.pack "c"),
              write (BSC.pack "d"),
              write (BSC.pack "efghijk"),
              write (BSC.pack "lmnoppq"),
              sync]
            ) initState

  res <- evalStateT (sequence [readByteString,
                               readByteString,
                               readByteString,
                               readByteString]) $ reset s

  mapM_ (putStrLn . BSC.unpack) res

  return ()
