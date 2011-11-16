{-# LANGUAGE OverloadedStrings #-}
-- -----------------------------------------------------------------------------
-- |
-- Module      :  Network.IRC.Message.Types
-- Copyright   :  (c) Andreas-Christoph Bernstein 2011
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  andreas.bernstein@googlemail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Types for working with IRC messages.

module Network.IRC.Message.Types
  (
    Message(..)
  , Command(..)
  , Prefix(..)
  , Params(..)
  ) where

import           Prelude hiding (takeWhile)
import           Control.Applicative hiding (many)
import           Data.Attoparsec
import qualified Data.Attoparsec.Char8 as P8
import           Data.Attoparsec.Char8 (char8)
import Data.Word (Word8)
import qualified Data.ByteString.Char8 as B hiding (map)
import qualified Data.ByteString as BS

data Message = Message !(Maybe Prefix) !Command !Params
  deriving (Eq, Ord, Show)

type User = B.ByteString

data Prefix = PrefixServer !Servername
            | PrefixNick !Nick !(Maybe User) !(Maybe Host)
  deriving (Eq,Ord,Show)

data Command =
    CmdNumericReply Int
  | ADMIN
  | AWAY
  | CONNECT
  | DIE
  | ERROR
  | INFO
  | INVITE
  | ISON
  | JOIN
  | KICK
  | KILL
  | LINKS
  | LIST
  | LUSERS
  | MODE
  | MOTD
  | NAMES
  | NICK
  | NOTICE
  | OPER
  | PART
  | PASS
  | PING
  | PONG
  | PRIVMSG
  | QUIT
  | REHASH
  | RESTART
  | SERVICE
  | SERVLIST
  | SQUERY
  | SQUIT
  | STATS
  | SUMMON
  | TIME
  | TOPIC
  | TRACE
  | USER
  | USERHOST
  | USERS
  | VERSION
  | WALLOPS
  | WHO
  | WHOIS
  | WHOWAS
  deriving (Show,Read,Eq,Ord)

data Params = Params [B.ByteString] (Maybe B.ByteString)
  deriving (Eq,Ord,Show,Read)

type Mask = B.ByteString
type Target = [To]
data To = ToChannel Channel
        | ToUserAtServer User Servername
        | ToNick Nick
        | ToMask Mask

type Channel = B.ByteString
type Servername = Host
type Host = B.ByteString
type Nick = B.ByteString
