{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module JournalFile (createJournalFile, write, sync) where

import System.IO
import Foreign.Ptr
import Foreign.Storable
import System.Posix.Memory
import System.Posix.IO
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Debug.Trace

type Journal a = StateT (MemoryMappedFile a) IO

data MemoryMappedFile a = MMF {
    startingPtr :: Ptr a
  , currentPtr :: Ptr a
}

allPermissions :: [MemoryProtection]
allPermissions = [MemoryProtectionRead, MemoryProtectionWrite, MemoryProtectionExecute]

createJournalFile :: FilePath -> Integer -> IO (MemoryMappedFile a)
createJournalFile fp size = do
  h <- openBinaryFile fp ReadWriteMode
  hSetFileSize h size
  hClose h
  fd <- openFd fp ReadWrite Nothing defaultFileFlags
  ptr <- memoryMap Nothing (fromInteger size) allPermissions MemoryMapShared (Just fd) 0
  return (MMF ptr ptr)

write :: Storable a => a -> Journal a ()
write x = do
  mmf <- get
  alignmentSize <- liftIO $ return (alignment x)
  liftIO $ trace("alignment size = " ++ show alignmentSize) return ()
  liftIO $ pokeElemOff (currentPtr mmf) 0 x
  put mmf { currentPtr = plusPtr (currentPtr mmf) (sizeOf x) }
  return ()

sync :: Journal a ()
sync = do
  mmf <- get
  liftIO $ memorySync (startingPtr mmf) 4096 [MemorySyncSync]
  return ()

openTempJournalFile :: FilePath -> String -> Integer -> IO FilePath
openTempJournalFile fpTemplate s size = do
  (fp,h) <- openBinaryTempFile fpTemplate s
  hSetFileSize h size
  hClose h
  return fp
