{-# LANGUAGE OverloadedStrings #-}
-- -----------------------------------------------------------------------------
-- |
-- Module      :  Ircfs.Ctl
-- Copyright   :  (c) Andreas-Christoph Bernstein 2011
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  andreas.bernstein@googlemail.com
-- Stability   :  unstable
-- Portability :  not portable
--
-- Ctl Commands
--
--------------------------------------------------------------------------------

module Ircfs.Ctl
  (
    parseCtl
  , toMessage
  , CtlCommand(..)
  ) where

import           Control.Applicative hiding (many)
import qualified Data.ByteString as B
import qualified Data.Attoparsec as A
import qualified Data.Attoparsec.Char8 as A8
import qualified Network.IRC.Message as I
import qualified Network.IRC.Message.Parser as I

data CtlCommand =
    Away { msg :: B.ByteString }
  | Back
  | Ban
  | Connect { server :: B.ByteString, nick :: B.ByteString}
  | Ctcp
  | Debug
  | Deop
  | Devoice
  | Disconnect
  | Invite
  | Join { chan :: B.ByteString }
  | Kick
  | Me B.ByteString
  | Mode { nickOrChan :: B.ByteString , modes :: [B.ByteString] }
  | Motd { target :: Maybe B.ByteString }
  | Names { msg :: B.ByteString }
  | Nick { nickname :: B.ByteString }
  | Notice { msgtarget :: B.ByteString, msg :: B.ByteString }
  | Op
  | Part { msg :: B.ByteString }
  | Ping { msg :: B.ByteString } 
  | Pong { msg :: B.ByteString } 
  | Privmsg { msgtarget :: B.ByteString, msg :: B.ByteString }
  | Quit { msg :: B.ByteString }
  | Reconnect
  | Remove B.ByteString
  | Time { target :: Maybe B.ByteString }
  | Topic
  | Umode
  | Unban
  | Voice
  | Who { name :: !B.ByteString } 
  | Unknown !B.ByteString
  deriving (Show, Eq, Ord)

parseCtl :: A.Parser CtlCommand
parseCtl = awayCmd
      <|> backCmd
      -- ban
      <|> connectCmd
      -- ctcp
      <|> debugCmd
      <|> disconnectCmd
      <|> joinCmd
      <|> meCmd
      <|> msgCmd
      <|> namesCmd
      <|> nCmd
      <|> nickCmd
      <|> partCmd
      <|> pongCmd
      <|> quitCmd
      -- <|> removeCmd
      <|> whoCmd
      <|> unknownCmd

awayCmd :: A8.Parser CtlCommand
awayCmd = Away <$> (A8.string "away" *> A8.skipSpace *> remainder)
backCmd :: A8.Parser CtlCommand
backCmd = Back <$ A8.string "back"
connectCmd :: A8.Parser CtlCommand
connectCmd = Connect <$> (A8.string "connect" *> A8.skipSpace *> I.host)
                      <*> (A8.skipSpace *> I.nick)
debugCmd :: A8.Parser CtlCommand
debugCmd = Debug <$ A8.string "debug"
disconnectCmd :: A8.Parser CtlCommand
disconnectCmd = Disconnect <$ A8.string "disconnect"
joinCmd :: A8.Parser CtlCommand
joinCmd = Join <$> (A8.string "join" *> A8.skipSpace *> I.channel)
meCmd :: A8.Parser CtlCommand
meCmd = Me <$> (A8.string "me" *> A8.skipSpace *> remainder)
msgCmd :: A8.Parser CtlCommand
msgCmd = Privmsg <$> (A8.string "msg" *> A8.skipSpace *> (I.nick <|> I.channel))
                <*> (A8.skipSpace *> remainder)
namesCmd :: A8.Parser CtlCommand
namesCmd = Names <$> (A8.string "names" *> A8.skipSpace *> remainder)
nCmd :: A8.Parser CtlCommand
nCmd = Names <$> (A8.string "n" *> A8.skipSpace *> remainder)
nickCmd :: A8.Parser CtlCommand
nickCmd = Nick <$> (A8.string "nick" *> A8.skipSpace *> I.nick)
partCmd :: A8.Parser CtlCommand
partCmd = Part <$> (A8.string "part" *> A8.skipSpace *> remainder)
pongCmd :: A8.Parser CtlCommand
pongCmd = Pong <$> (A8.string "pong" *> A8.skipSpace *> remainder)
quitCmd :: A8.Parser CtlCommand
quitCmd = Quit <$> (A8.string "quit" *> A8.skipSpace *> remainder)
--removeCmd = Remove <$> (A8.string "remove" *> A8.skipSpace *> remainder)
whoCmd :: A8.Parser CtlCommand
whoCmd = Who <$> (A8.string "whois" *> A8.skipSpace *> I.nick)
unknownCmd :: A8.Parser CtlCommand
unknownCmd = Unknown <$> remainder

remainder :: A.Parser B.ByteString
remainder = A.takeTill A8.isEndOfLine

--timeCmd    = Time    <$> (string "time"    *> optional (space *> undefined))
--pingCmd    = Ping    <$> (string "ping"    *> space *> undefined)

-- upperCaseFirstWord

-- from rfc2812 :
-- "Clients SHOULD NOT use a prefix when sending a
-- message; if they use one, the only valid prefix is the registered
-- nickname associated with the client.
-- The command MUST either be a valid IRC command or a three (3) digit
-- number represented in ASCII text.
--
-- IRC messages are always lines of characters terminated with a CR-LF
-- (Carriage Return - Line Feed) pair, and these messages SHALL NOT
-- exceed 512 characters in length, counting all characters including
-- the trailing CR-LF. Thus, there are 510 characters maximum allowed
-- for the command and its parameters.  There is no provision for
-- continuation of message lines.  See section 6 for more details about
-- current implementations.
-- "

-- XXX : TODO testing
toMessage :: CtlCommand -> {- Maybe -} I.Message
toMessage (Away s) = I.Message Nothing I.AWAY (I.Params [s] Nothing)
toMessage Back     = I.Message Nothing I.AWAY (I.Params [] Nothing)
toMessage (Connect s n) = I.Message Nothing I.ERROR (I.Params ["connect command"] Nothing)
toMessage Debug = I.Message Nothing I.ERROR (I.Params ["debug command"] Nothing)
toMessage Disconnect = I.Message Nothing I.ERROR (I.Params ["disconnect command"] Nothing)
toMessage (Join s) = I.Message Nothing I.JOIN (I.Params [s] Nothing)
--toMessage (Me s)   = I.Message Nothing I.JOIN [s]
toMessage (Names s) = I.Message Nothing I.NAMES (I.Params [s] Nothing)
toMessage (Nick s) = I.Message Nothing I.NICK (I.Params [s] Nothing)
toMessage (Part s) = I.Message Nothing I.PART (I.Params [s] Nothing)
toMessage (Pong s) = I.Message Nothing I.PONG (I.Params [s] Nothing)
toMessage (Privmsg t s) = I.Message Nothing I.PRIVMSG (I.Params [t] (Just s))
toMessage (Quit s) = I.Message Nothing I.QUIT (I.Params [s] Nothing)
--toMessage (Remove s) = I.Message Nothing I.REMOVE [s]
toMessage (Who s) = I.Message Nothing I.WHO (I.Params [s] Nothing)
toMessage _ = I.Message Nothing I.ERROR (I.Params ["unknown CtlCommand"] Nothing)

