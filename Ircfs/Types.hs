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
-- IrcfsState
-- io
-- runIrcfs
-- Ircfs
-- Qreq
-- Connection
-- File
-- Target
-- Targets
--
-- connectionLens
-- addrLens
-- nickLens
where

import Prelude hiding ((.), id)
import Control.Category
import Control.Applicative
import qualified System.Posix.Types as S
import qualified Data.ByteString.Char8 as B
import qualified Network.Socket as N hiding (recv)
import qualified Data.Rope as R
import qualified Control.Concurrent.Chan as C
import qualified Control.Concurrent as C
import Control.Monad.State
import qualified System.Fuse as F
import qualified Data.Lens.Common as L
import Data.IntMap
import qualified Data.Map as M

-- | IrcfsState, the irc file system state.
data IrcfsState = IrcfsState
    { connection :: Connection
    -- , fsreq :: C.Chan FsRequest -- > move to IrcfsState
    } 

connectionLens :: L.Lens IrcfsState Connection
connectionLens = L.lens connection (\x s -> s { connection = x })

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

-- |
-- A Connection
--
data Connection = 
    NotConnected
  | Connection
    { addr :: File
    , nick :: File
    --, lnick :: String
    , targets :: Targets -- M.Map Int Target
    , sock :: N.Socket
    -- readable Files in the root dir
    -- , ctlFile :: B.ByteString -- reading provides command history ?
    -- , commandHistoryFile
    , eventFile :: File -- everything
    , pongFile :: File -- every time a pong is send
    , rawFile :: File
    , nextDirNames :: [Int]
    , targetMap :: M.Map B.ByteString Int -- map directory number to target id
    }

type File = B.ByteString

--ctlLens :: L.Lens Connection File
--ctlLens = L.lens ctlFile (\x s -> s { ctlFile = x })
addrLens :: L.Lens Connection File
addrLens = L.lens addr (\x s -> s { addr = x })
nickLens :: L.Lens Connection File
nickLens = L.lens nick (\x s -> s { nick = x })
targetsLens :: L.Lens Connection Targets
targetsLens = L.lens targets (\x s -> s { targets = x })
targetLens :: Int -> L.Lens Connection (Maybe Target)
targetLens k = L.intMapLens k . targetsLens
sockLens :: L.Lens Connection N.Socket
sockLens = L.lens sock (\x s -> s { sock = x })
eventLens :: L.Lens Connection File
eventLens = L.lens eventFile (\x s -> s { eventFile = x })
pongLens :: L.Lens Connection File
pongLens = L.lens pongFile (\x s -> s { pongFile = x })
rawLens :: L.Lens Connection File
rawLens = L.lens rawFile (\x s -> s { rawFile = x })
nextDirNamesLens :: L.Lens Connection [Int]
nextDirNamesLens = L.lens nextDirNames (\x s -> s { nextDirNames = x})

targetMapLens :: L.Lens Connection (M.Map B.ByteString Int)
targetMapLens = L.lens targetMap (\x s -> s { targetMap = x})

targetMapLens' :: B.ByteString -> L.Lens Connection (Maybe Int)
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
    , targetName :: File
    -- , users :: [String] 
    , users :: File
    --, text :: R.Rope
    , text :: File
    } deriving (Show, Eq)

tagLens :: L.Lens Target Int
tagLens = L.lens tag (\x s -> s { tag = x })
toLens :: L.Lens Target To
toLens = L.lens to (\x s -> s { to = x })
nameLens :: L.Lens Target File
nameLens = L.lens targetName (\x s -> s { targetName = x })
--usersLens :: L.Lens Target [String]
usersLens :: L.Lens Target File
usersLens = L.lens users (\x s -> s { users = x })
--textLens :: L.Lens Target R.Rope
textLens :: L.Lens Target File
textLens = L.lens text (\x s -> s { text = x })

data To = TChannel | TUser
  deriving (Show, Read, Eq)

-- data In  = FsRequest F.Request
--          | Cmd CtlCommand
--          | Irc I.Message
--          | Shutdown
--          deriving (Eq)
