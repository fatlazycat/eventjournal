{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module JournalFile (createJournalFile, write, sync, openTempFile) where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
import           Debug.Trace
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           Foreign.Storable
import           System.IO
import           System.IO.MMap
import qualified System.Posix.IO           as PIO

type Journal a = StateT (MemoryMappedFile a) IO

data MemoryMappedFile a = MMF {
    memoryPtr      :: ForeignPtr a
  , startingOffset :: Int
  , size           :: Int
  , currentOffset  :: Int
}

createJournalFile :: FilePath -> Integer -> IO (MemoryMappedFile a)
createJournalFile fp size = do
  (fPtr, offset, size) <- mmapFileForeignPtr fp ReadWriteEx Just(0, size)
  return (MMF fPtr offset size offset)

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

openTempFile :: FilePath -> String -> Integer -> IO FilePath
openTempFile fpTemplate s size = do
  (fp,h) <- openBinaryTempFile fpTemplate s
  hSetFileSize h size
  hClose h
  return fp
