{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Monad.Trans.State
import qualified Data.ByteString.Char8     as BSC
import           Data.Serialize
import           Event
import           JournalFile

main :: IO ()
main = do
  initState <- createJournalFile "test.data" 1024
  let sampleEvent1 = Event 1 1 1 1 1 $ BSC.pack "blah"
  let sampleEvent2 = Event 2 2 2 2 2 $ BSC.pack "more blah"

  s <- execStateT (
    sequence [
              write $ encode sampleEvent1,
              write $ encode sampleEvent2,
              sync]
            ) initState

  res <- evalStateT (sequence [
                               readByteString,
                               readByteString]) $ reset s

  let decodeEvents :: [Either String Event] = map decode res
  mapM_ print decodeEvents

  return ()
