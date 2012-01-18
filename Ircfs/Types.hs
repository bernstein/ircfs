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
    IrcfsState(..)
  , runIrcfs
  , Ircfs(..)
  , io
  , Qreq(..)
  , File
  , Target(..)
  , Targets
  , To(..)

  , defaultFileStat
  , defaultDirStat
  --, connectionLens
  , addrLens
  , nickLens
  , targetLens
  , tTagL
  , eventLens
  , pongLens
  , targetMapLens
  , targetMapLens'
  , rawLens
  , nextDirNamesLens
  , IrcOut(..)
  , liftLens
  , inodesL
  , inodeL
  , statL
  , dataL

  , now
  ) where

import Prelude hiding ((.), id)
import Control.Category
import Control.Applicative
import qualified Data.ByteString.Char8 as B
import Foreign.C.Types (CTime)
--import qualified Network.IRC.Message as I
--import qualified Data.Rope as R
import qualified Control.Concurrent.Chan as C
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

-- | IrcfsState, the irc file system state.
data IrcfsState = IrcfsState
    { --connection :: Connection
      addr :: File
    , targets :: Targets -- M.Map Int Target
    , nextDirNames :: [Int]
    , targetMap :: M.Map B.ByteString Int -- map directory number to target id
    , userID :: CUid
    , groupID :: CGid
    , inodes :: M.Map Qreq Inode
    , start :: CTime
    } 

io :: MonadIO m => IO a -> m a
io = liftIO

runIrcfs :: IrcfsState -> Ircfs a -> IO (a, IrcfsState)
runIrcfs st (Ircfs a) = runStateT a st

-- | The Ircfs monad, 'StateT' transformer over 'IO'
-- encapsulating the ircfs state.
newtype Ircfs a = Ircfs (StateT IrcfsState IO a)
  deriving (Functor, Monad, MonadIO, MonadState IrcfsState)

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
          | Qdir   { dirNr :: Int } -- "/n"
          | Qctl   { dirNr :: Int } -- "/n/ctl"
          | Qname  { dirNr :: Int } -- "/n/name"
          | Qusers { dirNr :: Int } -- "/n/users"
          | Qdata  { dirNr :: Int } -- "/n/data"
  deriving (Show, Read, Eq, Ord)

type File = B.ByteString

--ctlLens :: L.Lens Connection File
--ctlLens = L.lens ctlFile (\x s -> s { ctlFile = x })
 
addrLens :: L.Lens IrcfsState File
addrLens = L.lens addr (\x s -> s { addr = x })

nickLens :: L.Lens IrcfsState (Maybe File)
nickLens = dataL Qnick

targetsLens :: L.Lens IrcfsState Targets
targetsLens = L.lens targets (\x s -> s { targets = x })

targetLens :: Int -> L.Lens IrcfsState (Maybe Target)
targetLens k = L.intMapLens k . targetsLens

eventLens :: L.Lens IrcfsState (Maybe File)
eventLens = dataL Qevent

pongLens :: L.Lens IrcfsState (Maybe File)
pongLens = dataL Qpong

rawLens :: L.Lens IrcfsState (Maybe File)
rawLens = dataL Qraw

nextDirNamesLens :: L.Lens IrcfsState [Int]
nextDirNamesLens = L.lens nextDirNames (\x s -> s { nextDirNames = x})

targetMapLens :: L.Lens IrcfsState (M.Map B.ByteString Int)
targetMapLens = L.lens targetMap (\x s -> s { targetMap = x})

targetMapLens' :: B.ByteString -> L.Lens IrcfsState (Maybe Int)
targetMapLens' s = L.mapLens s . targetMapLens

type Targets = IntMap Target -- change to (IntMap Target)

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

inodesL :: L.Lens IrcfsState (M.Map Qreq Inode)
inodesL = L.lens inodes (\x s -> s { inodes = x })

inodeL :: Qreq -> L.Lens IrcfsState (Maybe Inode)
inodeL p = L.mapLens p . inodesL

statL :: Qreq -> L.Lens IrcfsState (Maybe F.FileStat)
statL p = liftLens iStatL . inodeL p

dataL :: Qreq -> L.Lens IrcfsState (Maybe B.ByteString)
dataL p = liftLens iDataL . inodeL p

defaultFileStat :: IrcfsState -> F.FileStat
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

defaultDirStat :: IrcfsState -> F.FileStat
defaultDirStat st = (defaultFileStat st)
                { F.statEntryType = F.Directory
                , F.statFileMode = 0o550
                , F.statLinkCount = 2
                , F.statFileSize = 4096
                }

now :: IO CTime
now = fromIntegral . truncate <$> T.getPOSIXTime
-- T.posixSecondsToUTCTime . realToFrac <$> now

-- data In  = FsRequest F.Request
--          | Cmd CtlCommand
--          | Irc I.Message
--          | Shutdown
--          deriving (Eq)
