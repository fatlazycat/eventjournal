module Event where

import Data.ByteString
import Data.Serialize

data Event = Event {
    aggregateId :: Integer
  , aggregateVersion :: Integer
  , uniqueId :: Integer
  , timestamp :: Integer
  , eventTypeId :: Int
  , payload :: ByteString
} deriving (Show)

instance Serialize Event where
  put (Event aId aVer uId t eId p) = do
    put aId
    put aVer
    put uId
    put t
    put eId
    put p
  get = do
    aId <- get
    aVer <- get
    uId <- get
    t <- get
    eId <- get
    p <- get
    return (Event aId aVer uId t eId p)
