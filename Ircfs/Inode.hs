-- -----------------------------------------------------------------------------
-- |
-- Module      :  Ircfs.Inode
-- Copyright   :  (c) Andreas-Christoph Bernstein 2012
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  andreas.bernstein@googlemail.com
-- Stability   :  unstable
-- Portability :  not portable
--
-- file system node
--
--------------------------------------------------------------------------------

module Ircfs.Inode
  (
    Inode(..)
  , iStatL
  , iDataL
  , FileData
  , mkTextFileData
  , mkInode
  , iStatEntryTypeL
  , iStatFileModeL
  , iStatLinkCountL
  , iStatFileOwnerL
  , iStatFileGroupL
  , iStatSpecialDeviceIDL
  , iStatFileSizeL
  , iStatBlocksL
  , iStatAccessTimeL
  , iStatModificationTimeL
  , iStatStatusChangeTimeL
  , chown, chgrp, chmod, setTimes
  ) where

import Prelude hiding ((.), id)
import Control.Category
import Control.Applicative
import Foreign.C.Types (CTime)
import qualified System.Fuse as F
import qualified System.Posix.Types as S
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import           Data.Monoid
import           Data.Lens.Common
import           Ircfs.Misc

data Inode = Inode
  {
    iStat :: F.FileStat
  , iData :: FileData
  }
  deriving (Show)

mkTextFileData :: B.ByteString -> FileData
--mkTextFileData = L.fromChunks . pure
mkTextFileData = id
type FileData = B.ByteString

mkInode ::  F.FileStat -> Inode
mkInode st = Inode {iStat = st, iData = mempty}

instance Eq Inode where
  (Inode s d) == (Inode s' d') = d==d' 

iStatL :: Lens Inode F.FileStat
iStatL = lens iStat (\x s -> s { iStat = x })

iDataL :: Lens Inode FileData
iDataL = lens iData (\x s -> s { iData = x })

statEntryTypeL :: Lens F.FileStat F.EntryType
statEntryTypeL = lens F.statEntryType (\x s -> s { F.statEntryType = x })

statFileModeL :: Lens F.FileStat S.FileMode
statFileModeL = lens F.statFileMode (\x s -> s { F.statFileMode = x })

statLinkCountL :: Lens F.FileStat S.LinkCount
statLinkCountL = lens F.statLinkCount (\x s -> s { F.statLinkCount = x })

statFileOwnerL :: Lens F.FileStat S.UserID
statFileOwnerL = lens F.statFileOwner (\x s -> s { F.statFileOwner = x })

statFileGroupL :: Lens F.FileStat S.GroupID
statFileGroupL = lens F.statFileGroup (\x s -> s { F.statFileGroup = x })

statSpecialDeviceIDL :: Lens F.FileStat S.DeviceID
statSpecialDeviceIDL = lens F.statSpecialDeviceID (\x s -> s { F.statSpecialDeviceID = x })

statFileSizeL :: Lens F.FileStat S.FileOffset
statFileSizeL = lens F.statFileSize (\x s -> s { F.statFileSize = x })

statBlocksL :: Lens F.FileStat Integer
statBlocksL = lens F.statBlocks (\x s -> s { F.statBlocks = x })

statAccessTimeL :: Lens F.FileStat S.EpochTime
statAccessTimeL = lens F.statAccessTime (\x s -> s { F.statAccessTime = x })

statModificationTimeL :: Lens F.FileStat S.EpochTime
statModificationTimeL = lens F.statModificationTime (\x s -> s { F.statModificationTime = x })

statStatusChangeTimeL :: Lens F.FileStat S.EpochTime
statStatusChangeTimeL = lens F.statStatusChangeTime (\x s -> s { F.statStatusChangeTime = x })

iStatEntryTypeL :: Lens Inode F.EntryType
iStatEntryTypeL = statEntryTypeL . iStatL

iStatFileModeL :: Lens Inode S.FileMode
iStatFileModeL = statFileModeL . iStatL

iStatLinkCountL :: Lens Inode S.LinkCount
iStatLinkCountL = statLinkCountL . iStatL

iStatFileOwnerL :: Lens Inode S.UserID
iStatFileOwnerL = statFileOwnerL . iStatL

iStatFileGroupL :: Lens Inode S.GroupID
iStatFileGroupL = statFileGroupL . iStatL

iStatSpecialDeviceIDL :: Lens Inode S.DeviceID
iStatSpecialDeviceIDL = statSpecialDeviceIDL . iStatL

iStatFileSizeL :: Lens Inode S.FileOffset
iStatFileSizeL = statFileSizeL . iStatL

iStatBlocksL :: Lens Inode Integer
iStatBlocksL = statBlocksL . iStatL

iStatAccessTimeL :: Lens Inode S.EpochTime
iStatAccessTimeL = statAccessTimeL . iStatL

iStatModificationTimeL :: Lens Inode S.EpochTime
iStatModificationTimeL = statModificationTimeL . iStatL

iStatStatusChangeTimeL :: Lens Inode S.EpochTime
iStatStatusChangeTimeL = statStatusChangeTimeL . iStatL

chmod :: S.FileMode -> Endomorphism Inode
chmod = setL iStatFileModeL

chown :: S.UserID -> Endomorphism Inode
chown = setL iStatFileOwnerL

chgrp :: S.GroupID -> Endomorphism Inode
chgrp = setL iStatFileGroupL

setTimes :: S.EpochTime -> Endomorphism Inode
setTimes time =
      setL iStatModificationTimeL time
    . setL iStatAccessTimeL time
    . setL iStatStatusChangeTimeL time
