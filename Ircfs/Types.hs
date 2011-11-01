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
where

import Control.Applicative
import qualified System.Posix.Types as S
import qualified Data.ByteString.Char8 as B
import qualified Network.Socket as N hiding (recv)
import qualified Data.Rope as R
import qualified Control.Concurrent.Chan as C
import qualified Control.Concurrent as C
import Control.Monad.State
import qualified System.Fuse as F

-- | IrcfsState, the irc file system state.
data IrcfsState = IrcfsState
    { connection :: Connection
    -- , fsreq :: C.Chan FsRequest -- > move to IrcfsState
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
data Qreq = Qroot
          | Qrootctl
          | Qevent
          | Qraw
          | Qnick
          | Qpong
          | Qdir
          | Qctl
          | Qname
          | Qusers
          | Qdata
  deriving (Show, Read, Eq, Ord)

-- |
-- A Connection
--
data Connection = 
    NotConnected
  | Connection
    { addr :: String
    , nick :: B.ByteString
    --, lnick :: String
    , targets :: Targets -- M.Map Int Target
    , sock :: N.Socket
    -- readable Files in the root dir
    -- , ctlFile :: B.ByteString -- reading provides command history ?
    -- , commandHistoryFile
    , eventFile :: B.ByteString -- everything
    , pongFile :: B.ByteString -- every time a pong is send
    , rawFile :: B.ByteString
    }

type Targets = [Target]

-- findTag 
-- findTarget

-- |
-- A Target 
--
data Target = Target 
    { tag :: !Int
    , to :: To
    , name :: String
    , users :: [String] 
    , text :: R.Rope
    } deriving (Show, Eq)

data To = TChannel | TUser
  deriving (Show, Read, Eq)

{-

data IrcfsState = App
  {
    con :: Connection
  } deriving (Show, Eq, Ord)
-} 

