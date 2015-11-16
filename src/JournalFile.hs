module JournalFile (createJournalFile, currentWriteLocation) where

import System.IO
import Foreign.Ptr
import System.Posix.Memory
import System.Posix.IO
import Control.Monad.State.Strict

newtype Journal a = State (MemoryMappedFile a)

data MemoryMappedFile a = MMF (Ptr a) (Ptr a)

currentWriteLocation (MMF ptr _) = ptr

allPermissions = [MemoryProtectionRead, MemoryProtectionWrite, MemoryProtectionExecute]

createJournalFile :: FilePath -> Integer -> IO (MemoryMappedFile a)
createJournalFile fp size = do
  h <- openBinaryFile fp ReadWriteMode
  hSetFileSize h size
  hClose h
  fd <- openFd fp ReadWrite Nothing defaultFileFlags
  ptr <- memoryMap Nothing (fromInteger size) allPermissions MemoryMapShared (Just fd) 0
  return (MMF ptr ptr)

openTempJournalFile :: FilePath -> String -> Integer -> IO FilePath
openTempJournalFile fpTemplate s size = do
  (fp,h) <- openBinaryTempFile fpTemplate s
  hSetFileSize h size
  hClose h
  return fp
