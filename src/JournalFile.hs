{-# LANGUAGE ScopedTypeVariables #-}
module JournalFile (createJournalFile, write, sync, syncPoints, openTempJournalFile) where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
import           Data.ByteString
import qualified Data.ByteString.Internal  as BSI
import qualified Data.Int                  as DI
import qualified Data.Primitive.MachDeps   as DPM
import           Debug.Trace
import qualified Foreign.C.Types           as FCT
import           Foreign.ForeignPtr
import qualified Foreign.Marshal.Utils     as FMU
import           Foreign.Ptr
import           Foreign.Storable
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
  liftIO $ trace("bsLength = " ++ show bsLength ++ ", length of BS = " ++ show (Data.ByteString.length x)) return ()
  liftIO $ withForeignPtr bsFPtr
                          (\bsPtr -> withForeignPtr (memoryFPtr mmf)
                                                    (\memPtr -> let src = plusPtr bsPtr bsOffset
                                                                    dest = plusPtr memPtr (currentOffset mmf)
                                                                    destBS = plusPtr memPtr (currentOffset mmf + DPM.sIZEOF_INT64)
                                                                    lengthOfBS :: DI.Int64 = fromIntegral bsLength
                                                                in do
                                                                  poke dest lengthOfBS
                                                                  FMU.copyBytes destBS src bsLength))
  put mmf { currentOffset = currentOffset mmf + DPM.sIZEOF_INT64 + bsLength }
  return ()

sync :: Journal ByteString ()
sync = do
  mmf <- get
  let currentOffsetMMF = currentOffset mmf
  let numBytesToSync = currentOffsetMMF - lastSyncedOffset mmf
  liftIO $ withForeignPtr (memoryFPtr mmf)
                          (\memPtr -> let (syncPagedOffset, syncPagedSize) = syncPoints (lastSyncedOffset mmf) numBytesToSync SPM.sysconfPageSize
                                          syncPtr = plusPtr memPtr syncPagedOffset
                                          syncSize = FCT.CSize (fromIntegral syncPagedSize)
                                      in SPM.memorySync syncPtr syncSize [SPM.MemorySyncSync])
  put mmf { lastSyncedOffset = currentOffsetMMF + numBytesToSync }
  return ()

syncPoints :: Int -> Int -> Int -> (Int, Int)
syncPoints start size pageSize = (pagedStart, pagedSize)
                                 where pagedStart = quot start pageSize * pageSize
                                       pagedSize = (quot (start + size) pageSize + 1) * pageSize - pagedStart

openTempJournalFile :: FilePath -> String -> Integer -> IO FilePath
openTempJournalFile fpTemplate s size = do
  (fp,h) <- openBinaryTempFile fpTemplate s
  hSetFileSize h size
  hClose h
  return fp
