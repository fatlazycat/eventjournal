module JournalFile (createJournalFile, write, sync, openTempJournalFile) where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
import           Data.ByteString
import qualified Data.ByteString.Internal  as BSI
-- import           Debug.Trace
import qualified Foreign.C.Types           as FCT
import           Foreign.ForeignPtr
import qualified Foreign.Marshal.Utils     as FMU
import           Foreign.Ptr
import           System.IO
import           System.IO.MMap
import qualified System.Posix.Memory       as SPM

type Journal a = StateT (MemoryMappedFile a) IO

data MemoryMappedFile a = MMF {
    memoryFPtr       :: ForeignPtr a
  , startingOffset   :: Int
  , mappedSize       :: Int
  , currentOffset    :: Int
  , lastSyncedOffset :: Int
}

createJournalFile :: FilePath -> Int -> IO (MemoryMappedFile a)
createJournalFile fp size = do
  (fPtr, offset, createdSize) <- mmapFileForeignPtr fp ReadWriteEx (Just(0, size))
  return (MMF fPtr offset createdSize offset offset)

write :: ByteString -> Journal ByteString ()
write x = do
  mmf <- get
  let (bsFPtr, bsOffset, bsLength) = BSI.toForeignPtr x
  liftIO $ withForeignPtr bsFPtr
                          (\bsPtr -> withForeignPtr (memoryFPtr mmf)
                                                    (\memPtr -> let src = plusPtr bsPtr bsOffset
                                                                    dest = plusPtr memPtr (currentOffset mmf)
                                                                in FMU.copyBytes dest src bsLength))
  put mmf { currentOffset = currentOffset mmf + bsLength }
  return ()

-- Implement boundary syncing on the alignement size for a sync
sync :: Journal ByteString ()
sync = do
  mmf <- get
  let numBytesToSync = currentOffset mmf - lastSyncedOffset mmf
  liftIO $ withForeignPtr (memoryFPtr mmf)
                          (\memPtr -> let syncPtr = plusPtr memPtr (lastSyncedOffset mmf)
                                          syncSize = FCT.CSize (fromIntegral numBytesToSync)
                                      in SPM.memorySync syncPtr syncSize [SPM.MemorySyncSync])
  return ()

openTempJournalFile :: FilePath -> String -> Integer -> IO FilePath
openTempJournalFile fpTemplate s size = do
  (fp,h) <- openBinaryTempFile fpTemplate s
  hSetFileSize h size
  hClose h
  return fp
