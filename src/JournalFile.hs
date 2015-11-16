{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module JournalFile (createJournalFile, currentWriteLocation) where

import System.IO
import Foreign.Ptr
import Foreign.Storable
import System.Posix.Memory
import System.Posix.IO
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as C

type Journal p = StateT (MemoryMappedFile p) IO

data MemoryMappedFile p = MMF {
    startingPtr :: Ptr p
  , currentPtr :: Ptr p
}

currentWriteLocation (MMF _ ptr) = ptr

allPermissions = [MemoryProtectionRead, MemoryProtectionWrite, MemoryProtectionExecute]

createJournalFile :: FilePath -> Integer -> Journal p ()
createJournalFile fp size = do
  h <- liftIO $ openBinaryFile fp ReadWriteMode
  liftIO $ hSetFileSize h size
  liftIO $ hClose h
  fd <- liftIO $ openFd fp ReadWrite Nothing defaultFileFlags
  ptr <- liftIO $ memoryMap Nothing (fromInteger size) allPermissions MemoryMapShared (Just fd) 0
  put (MMF ptr ptr)
  return ()

writeChar :: Char -> Journal p ()
writeChar c = do
  mmf <- get
  liftIO $ pokeElemOff (currentPtr mmf) 0 (C.singleton c)
  put mmf { currentPtr = plusPtr (currentPtr mmf) (sizeOf c) }
  return ()

openTempJournalFile :: FilePath -> String -> Integer -> IO FilePath
openTempJournalFile fpTemplate s size = do
  (fp,h) <- openBinaryTempFile fpTemplate s
  hSetFileSize h size
  hClose h
  return fp
