{-# LANGUAGE OverloadedStrings #-}
-- -----------------------------------------------------------------------------
-- |
-- Module      :  Ircfs.Process
-- Copyright   :  (c) Andreas-Christoph Bernstein 2011
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  andreas.bernstein@googlemail.com
-- Stability   :  unstable
-- Portability :  not portable
--
-- Process incoming file system messages.
--------------------------------------------------------------------------------
module Ircfs.Process
  ( 
    --process
    processIrc
  , timeStamp
  ) where

import Prelude hiding ((.), id)
import Control.Category
import Control.Applicative
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.State (get, put, modify)
import qualified Control.Concurrent.Chan as C
import qualified Control.Concurrent as C
import qualified Data.Lens.Common as L
import qualified Data.ByteString.Char8 as B
import Data.Monoid
import Data.Maybe (maybeToList, fromMaybe)
import Data.Attoparsec as A
import qualified Data.Time as T
import System.Locale (defaultTimeLocale)
import Foreign.C.Types (CTime)

import qualified System.Fuse.Request as F

import qualified Network.IRC.Message as I
import Ircfs.Ctl as I
import Ircfs.Types
import Ircfs.Filesystem
import Ircfs.Misc

isPingMessage :: I.Message -> Bool
isPingMessage (I.Message _ I.PING (I.Params _ (Just _))) = True
isPingMessage _ = False

-- | Process incomming irc messages.
processIrc :: CTime -> I.Message -> Ircfs [I.Message]
-- example: PING :irc.funet.fi ; Ping message sent by server
processIrc time (I.Message _ I.PING (I.Params _ (Just p))) =
  let now st = B.pack $ stamp' (timeZone st) time
      text st = mconcat [now st," pong ",p,"\n"]
      msg = I.Message Nothing I.PONG (I.Params [p] Nothing)
      f st = (touch Qpong time . append Qpong (text st) ) st
  in  [msg] <$ modify f
processIrc time (I.Message (Just (I.PrefixNick n _ _)) I.NICK (I.Params [] (Just new))) =
  let yourNick = L.getL nickLens
      someone = append Qevent (mconcat [n," changed nick to ",new,"\n"])
             . touch Qevent time
      you =  touch Qevent time
           . append Qevent (mconcat ["You're now known as ",new,"\n"])
           . touch Qnick time
           . substitute Qnick new
  in  nomsg $ \st -> if Just n == yourNick st then you st else someone st
processIrc time (I.Message (Just (I.PrefixNick n _ _)) I.NICK (I.Params (new:_) _)) =
  let yourNick st = maybe mempty id (L.getL nickLens st) -- nick st
      you =  append Qevent (mconcat ["You're now known as ",new,"\n"])
           . touch Qnick time
           . touch Qevent time
           . substitute Qnick new
      someone = append Qevent (mconcat [n," changed nick to ",new,"\n"])
               . touch Qevent time
  in  nomsg $ \st -> if n == yourNick st then you st else someone st
processIrc time (I.Message (Just (I.PrefixNick n _ _)) I.JOIN (I.Params [] (Just c))) =
  let yourNick st = L.getL nickLens st
      ins = insertChannel c time
      -- someone = "add user to users file"
  in  nomsg $ \st -> if (Just n == yourNick st) then ins st else st
processIrc time (I.Message (Just (I.PrefixNick n _ _)) I.JOIN (I.Params (c:_) _)) =
  let yourNick st = L.getL nickLens st
      ins = insertChannel c time
      -- someone = "add user to users file"
  in  nomsg $ \st -> if (Just n == yourNick st) then ins st else st
processIrc time (I.Message (Just (I.PrefixNick n _ _)) I.PART (I.Params (c:_) _)) =
  let yourNick st = L.getL nickLens st
      rm = removeChannel c time
      -- someone = "say user left to users file"
  in  nomsg $ \st -> if (Just n == yourNick st) then rm st else st
processIrc time (I.Message Nothing I.PRIVMSG (I.Params (c:cs) t)) = do
  tm <- L.getL (targetMapLens' c) <$> get
  st <- get
  let n = maybe mempty id (L.getL nickLens st) -- nick st
      now st = B.pack $ stamp' (timeZone st) time
      ts = maybeToList t
  maybe (return ()) (\k -> do
      modify ( \st ->
          touch (Qdata k) time
        . append (Qdata k) (mconcat [now st," < ",n,"> ",B.unwords (cs++ts),"\n"])
        $ st)
      ) tm
  return []
processIrc time (I.Message (Just (I.PrefixNick n _ _)) I.PRIVMSG (I.Params (c:cs) t)) = do
  tm <- L.getL (targetMapLens' c) <$> get
  let ts = maybeToList t
      now st = B.pack $ stamp' (timeZone st) time
      f k st =  touch (Qdata k) time
        . append (Qdata k) (mconcat [now st," < ",n,"> ",B.unwords (cs++ts),"\n"])
        $ st

  maybe (return []) (nomsg . f) tm
processIrc time m@(I.Message _ I.ERROR _) = nomsg $
  touch Qevent time . append Qevent (mconcat ["error ",I.encode m,"\n"])
processIrc _ _ = return []

nomsg :: Endomorphism IrcfsState -> Ircfs [I.Message]
nomsg e = [] <$ modify e

-- processIrc :: Message -> IrcfsState -> IrcfsState

-- ping :: IrcfsState -> IrcfsState

timeStamp :: MonadIO m => m B.ByteString
timeStamp = do
  now <- liftIO T.getCurrentTime
  return . B.pack $ T.formatTime defaultTimeLocale "%H:%M" now

timeStamp' :: CTime -> B.ByteString
timeStamp' = B.pack . stamp . toUTCTime

privmsg :: [B.ByteString] -> B.ByteString -> I.Message
privmsg targets x = I.Message Nothing I.PRIVMSG (I.Params targets (Just x))

