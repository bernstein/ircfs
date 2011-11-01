{-# LANGUAGE OverloadedStrings #-}
-- -----------------------------------------------------------------------------
-- |
-- Module      :  Filesystem.Fuse
-- Copyright   :  (c) Andreas-Christoph Bernstein 2011
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  andreas.bernstein@googlemail.com
-- Stability   :  unstable
-- Portability :  not portable
--
-- Fuse callbacks
--
--------------------------------------------------------------------------------

module System.Fuse.Request
where

import qualified Data.ByteString.Char8 as B
import qualified System.Fuse as F
import Foreign.C.Error (Errno,eNOENT)
import qualified System.Posix.Types as S
import System.Posix.IO
import qualified Control.Concurrent as C

-- like a self-addressed envelope (SAE)
data Request = ReqRep Tmsg (C.MVar Rmsg) | Req Tmsg

data Tmsg = Tread !FilePath !S.ByteCount !S.FileOffset
          | Twrite !FilePath !B.ByteString !S.FileOffset 
          | Topen !FilePath !F.OpenMode !F.OpenFileFlags
          | Tstat !FilePath
          -- Twalk
  --deriving (Show, Read, Eq)

data Rmsg = Rread !B.ByteString 
          | Rwrite !S.ByteCount
          | Ropen
          | Rstat !F.FileStat
          -- Rwalk
          | Rerror
  -- deriving (Show, Read, Eq)



data IrcfsFH = IrcfsFH
  deriving (Show,Read,Eq)

fromRmsg :: Rmsg -> Either Errno B.ByteString
fromRmsg (Rread s) = Right s
fromRmsg Rerror = Left eNOENT

fuseRequest_ :: C.Chan Request -> Tmsg -> IO ()
fuseRequest_ c m = C.writeChan c (Req m)

fuseRequest :: C.Chan Request -> Tmsg -> IO Rmsg
fuseRequest c m = do
  mvar <- C.newEmptyMVar
  C.writeChan c (ReqRep m mvar)
  C.takeMVar mvar

defaultDirStat = defaultFileStat
               { F.statEntryType = F.Directory
               , F.statFileMode = 0o555
               , F.statLinkCount = 2
               , F.statFileSize = 4096
               }

defaultFileStat = F.FileStat 
                { F.statEntryType = F.RegularFile
                , F.statFileMode = 0o222
                , F.statLinkCount = 1
                , F.statFileOwner = 0
                , F.statFileGroup = 0
                , F.statSpecialDeviceID = 0
                , F.statFileSize = 0
                , F.statBlocks = 1
                , F.statAccessTime = 0
                , F.statModificationTime = 0
                , F.statStatusChangeTime = 0
                }

fsOpen :: C.Chan Request -> FilePath -> OpenMode -> OpenFileFlags -> 
            IO (Either Errno IrcfsFH)
fsOpen c p m f = do
  x <- fuseRequest c (Topen p m f)
  case x of
    Ropen -> return (Right IrcfsFH)
    Rerror -> return (Left F.eNOENT)

fsRead :: C.Chan Request -> FilePath -> IrcfsFH -> S.ByteCount ->
            S.FileOffset -> IO (Either Errno B.ByteString)
fsRead c p _ bc off = do
  x <- fuseRequest c (Tread p bc off)
  case x of
    Rread s -> return (Right s)
    Rerror -> return (Left F.eNOENT)

fsWrite :: C.Chan Request -> FilePath -> IrcfsFH -> 
          B.ByteString -> S.FileOffset -> IO (Either Errno S.ByteCount)
fsWrite c p h s off = do
  x <- fuseRequest c (Twrite p s off)
  case x of
    Rwrite n -> return (Right n)
    Rerror -> return (Left F.eNOENT)

fsStat :: C.Chan Request -> FilePath -> IO (Either Errno F.FileStat)
fsStat c p = do
  x <- fuseRequest c (Tstat p)
  case x of
    Rstat s -> return (Right s)
    Rerror -> return (Left F.eNOENT)

