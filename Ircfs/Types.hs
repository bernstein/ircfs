{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
-- -----------------------------------------------------------------------------
-- |
-- Module      :  Ircfs.Types
-- Copyright   :  (c) Andreas-Christoph Bernstein 2011
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  andreas.bernstein@googlemail.com
-- Stability   :  unstable
-- Portability :  not portable
--
-- Used Types
--
--------------------------------------------------------------------------------

module Ircfs.Types
  (
    Fs(..)
  , runIrcfs
  , Ircfs(..)
  , io
  , Qreq(..)
  , File
  , Target(..)
  , To(..)

  , defaultFileStat
  , defaultDirStat
  --, connectionLens
  , addrLens
  , nickLens
  , targetLens
  , targetsLens
  , tTagL
  , eventLens
  , pongLens
  , targetMapLens
  , targetMapLens'
  , rawLens
  , IrcOut(..)
  , liftLens
  , inodesL
  , inodeL
  , statL
  , dataL
  , timeZoneL
  , connectionL
  , Connection(..)

  , FH(..)
  ) where

import Prelude hiding ((.), id)
import Control.Category
import Control.Applicative
import qualified Data.ByteString.Char8 as B
import Foreign.C.Types (CTime)
import qualified Network.IRC.Message as I
import qualified Control.Concurrent.Chan as C
import qualified Control.Concurrent as C
import Control.Monad.State
import qualified Data.Lens.Common as L
import Data.IntMap
import qualified Data.Map as M
import Ircfs.Inode
import Data.Monoid
import qualified System.Fuse as F
import System.Posix.Types
import qualified Data.Time as T
import qualified Data.Time.Clock.POSIX as T
import Control.Concurrent (ThreadId)

data FH = FH
  deriving (Show,Read,Eq)

-- | Fs, the irc file system state.
data Fs = Fs
    {
      addr :: File
    , targets :: IntMap Target
    , targetMap :: M.Map B.ByteString Int -- map directory number to target id
    , userID :: CUid
    , groupID :: CGid
    , effectiveUserName :: String
    , inodes :: M.Map Qreq Inode
    , start :: CTime
    , timeZone :: T.TimeZone
    , inChan :: C.Chan (Either String I.Message)
    , connection :: Connection
    } 

data Connection
  = Disconnected
  | Connecting
  | Connected { thr :: C.ThreadId, out :: C.Chan B.ByteString }

instance Show Connection where
  show (Connected t _) = "Connected " ++ show t ++ "<Chan>"
  show Connecting = "Connecting"
  show Disconnected = "Disconnected"

instance Eq Connection where
  (Connected a _) == (Connected b _) = a == b
  Connecting == Connecting  = True
  Disconnected == Disconnected = True
  _ == _ = False

io :: MonadIO m => IO a -> m a
io = liftIO

runIrcfs :: Fs -> Ircfs a -> IO (a, Fs)
runIrcfs st (Ircfs a) = runStateT a st

-- | The Ircfs monad, 'StateT' transformer over 'IO'
-- encapsulating the ircfs state.
newtype Ircfs a = Ircfs (StateT Fs IO a)
  deriving (Functor, Monad, MonadIO, MonadState Fs)

instance Applicative Ircfs where
  pure = return
  (<*>) = ap

-- |
-- Kind of Files
--
data Qreq = Qroot      -- "/"
          | Qrootctl   -- "/ctl"
          | Qevent     -- "/event"
          | Qraw       -- "/raw"
          | Qnick      -- "/nick"
          | Qpong      -- "/pong"
          -- | Qaddr      -- "/addr"
          -- | Qlag      -- "/lag"
          | Qdir   { dirNr :: Int } -- "/n"
          | Qctl   { dirNr :: Int } -- "/n/ctl"
          | Qname  { dirNr :: Int } -- "/n/name"
          | Qusers { dirNr :: Int } -- "/n/users"
          | Qdata  { dirNr :: Int } -- "/n/data"
  deriving (Show, Read, Eq, Ord)

type File = B.ByteString

--ctlLens :: L.Lens Connection File
--ctlLens = L.lens ctlFile (\x s -> s { ctlFile = x })
 
connectionL :: L.Lens Fs Connection
connectionL = L.lens connection (\x s -> s { connection = x })

timeZoneL :: L.Lens Fs T.TimeZone
timeZoneL = L.lens timeZone (\x s -> s { timeZone = x })

addrLens :: L.Lens Fs File
addrLens = L.lens addr (\x s -> s { addr = x })

nickLens :: L.Lens Fs (Maybe FileData)
nickLens = dataL Qnick

targetsLens :: L.Lens Fs (IntMap Target)
targetsLens = L.lens targets (\x s -> s { targets = x })

targetLens :: Int -> L.Lens Fs (Maybe Target)
targetLens k = L.intMapLens k . targetsLens

eventLens :: L.Lens Fs (Maybe FileData)
eventLens = dataL Qevent

pongLens :: L.Lens Fs (Maybe FileData)
pongLens = dataL Qpong

rawLens :: L.Lens Fs (Maybe FileData)
rawLens = dataL Qraw

targetMapLens :: L.Lens Fs (M.Map B.ByteString Int)
targetMapLens = L.lens targetMap (\x s -> s { targetMap = x})

targetMapLens' :: B.ByteString -> L.Lens Fs (Maybe Int)
targetMapLens' s = L.mapLens s . targetMapLens

-- findTag 
-- findTarget

-- |
-- A Target 
--
data Target = Target 
    { tag :: !Int
    , to :: To
    } deriving (Show, Eq)

tTagL :: L.Lens Target Int
tTagL = L.lens tag (\x s -> s { tag = x })

--toLens :: L.Lens Target To
--toLens = L.lens to (\x s -> s { to = x })

data To = TChannel | TUser
  deriving (Show, Read, Eq)

newtype IrcOut = IrcOut { unIrcOut :: C.Chan B.ByteString }

liftLens :: Applicative f => L.Lens a b -> L.Lens (f a) (f b)
liftLens l = L.lens (fmap (L.getL l)) (liftA2 (L.setL l))

inodesL :: L.Lens Fs (M.Map Qreq Inode)
inodesL = L.lens inodes (\x s -> s { inodes = x })

inodeL :: Qreq -> L.Lens Fs (Maybe Inode)
inodeL p = L.mapLens p . inodesL

statL :: Qreq -> L.Lens Fs (Maybe F.FileStat)
statL p = liftLens iStatL . inodeL p

dataL :: Qreq -> L.Lens Fs (Maybe FileData)
dataL p = liftLens iDataL . inodeL p

defaultFileStat :: Fs -> F.FileStat
defaultFileStat st = F.FileStat 
                { F.statEntryType = F.RegularFile
                , F.statFileMode = 0o222
                , F.statLinkCount = 1
                , F.statFileOwner = userID st
                , F.statFileGroup = groupID st
                , F.statSpecialDeviceID = 0
                , F.statFileSize = 0
                , F.statBlocks = 1
                , F.statAccessTime = start st
                , F.statModificationTime = start st
                , F.statStatusChangeTime = start st
                }

defaultDirStat :: Fs -> F.FileStat
defaultDirStat st = (defaultFileStat st)
                { F.statEntryType = F.Directory
                , F.statFileMode = 0o550
                , F.statLinkCount = 2
                , F.statFileSize = 4096
                }

-- data In  = FsRequest F.Request
--          | Cmd CtlCommand
--          | Irc I.Message
--          | Shutdown
--          deriving (Eq)
