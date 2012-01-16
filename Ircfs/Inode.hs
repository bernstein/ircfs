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
  ) where

import Prelude hiding ((.), id)
import Control.Category
import Control.Applicative
import Foreign.C.Types (CTime)
import qualified System.Fuse as F
import qualified System.Posix.Types as S
import qualified Data.ByteString.Char8 as B
import Data.Monoid
import qualified Data.Lens.Common as L

data Inode = Inode
  {
    iStat :: F.FileStat
  , iData :: FileData
  }
  deriving (Show)

mkTextFileData :: B.ByteString -> FileData
mkTextFileData = id
type FileData = B.ByteString

mkInode st = Inode {iStat = st, iData = mempty}

instance Eq Inode where
  (Inode s d) == (Inode s' d') = d==d' 

iStatL :: L.Lens Inode F.FileStat
iStatL = L.lens iStat (\x s -> s { iStat = x })

iDataL :: L.Lens Inode FileData
iDataL = L.lens iData (\x s -> s { iData = x })

statEntryTypeL :: L.Lens F.FileStat F.EntryType
statEntryTypeL = L.lens F.statEntryType (\x s -> s { F.statEntryType = x })

statFileModeL :: L.Lens F.FileStat S.FileMode
statFileModeL = L.lens F.statFileMode (\x s -> s { F.statFileMode = x })

statLinkCountL :: L.Lens F.FileStat S.LinkCount
statLinkCountL = L.lens F.statLinkCount (\x s -> s { F.statLinkCount = x })

statFileOwnerL :: L.Lens F.FileStat S.UserID
statFileOwnerL = L.lens F.statFileOwner (\x s -> s { F.statFileOwner = x })

statFileGroupL :: L.Lens F.FileStat S.GroupID
statFileGroupL = L.lens F.statFileGroup (\x s -> s { F.statFileGroup = x })

statSpecialDeviceIDL :: L.Lens F.FileStat S.DeviceID
statSpecialDeviceIDL = L.lens F.statSpecialDeviceID (\x s -> s { F.statSpecialDeviceID = x })

statFileSizeL :: L.Lens F.FileStat S.FileOffset
statFileSizeL = L.lens F.statFileSize (\x s -> s { F.statFileSize = x })

statBlocksL :: L.Lens F.FileStat Integer
statBlocksL = L.lens F.statBlocks (\x s -> s { F.statBlocks = x })

statAccessTimeL :: L.Lens F.FileStat S.EpochTime
statAccessTimeL = L.lens F.statAccessTime (\x s -> s { F.statAccessTime = x })

statModificationTimeL :: L.Lens F.FileStat S.EpochTime
statModificationTimeL = L.lens F.statModificationTime (\x s -> s { F.statModificationTime = x })

statStatusChangeTimeL :: L.Lens F.FileStat S.EpochTime
statStatusChangeTimeL = L.lens F.statStatusChangeTime (\x s -> s { F.statStatusChangeTime = x })

iStatEntryTypeL :: L.Lens Inode F.EntryType
iStatEntryTypeL = statEntryTypeL . iStatL

iStatFileModeL :: L.Lens Inode S.FileMode
iStatFileModeL = statFileModeL . iStatL

iStatLinkCountL :: L.Lens Inode S.LinkCount
iStatLinkCountL = statLinkCountL . iStatL

iStatFileOwnerL :: L.Lens Inode S.UserID
iStatFileOwnerL = statFileOwnerL . iStatL

iStatFileGroupL :: L.Lens Inode S.GroupID
iStatFileGroupL = statFileGroupL . iStatL

iStatSpecialDeviceIDL :: L.Lens Inode S.DeviceID
iStatSpecialDeviceIDL = statSpecialDeviceIDL . iStatL

iStatFileSizeL :: L.Lens Inode S.FileOffset
iStatFileSizeL = statFileSizeL . iStatL

iStatBlocksL :: L.Lens Inode Integer
iStatBlocksL = statBlocksL . iStatL

iStatAccessTimeL :: L.Lens Inode S.EpochTime
iStatAccessTimeL = statAccessTimeL . iStatL

iStatModificationTimeL :: L.Lens Inode S.EpochTime
iStatModificationTimeL = statModificationTimeL . iStatL

iStatStatusChangeTimeL :: L.Lens Inode S.EpochTime
iStatStatusChangeTimeL = statStatusChangeTimeL . iStatL
