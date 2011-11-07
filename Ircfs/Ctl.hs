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
  ) where

import           Control.Applicative hiding (many)
import qualified Data.ByteString as B
import qualified Data.Attoparsec as A
import qualified Data.Attoparsec.Char8 as A8
import qualified Network.IRC.Message as I

data CtlCommand =
    Away { msg :: B.ByteString }
  | Back
  | Ban
  | Connect
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
      -- connect
      -- ctcp
      <|> debugCmd
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

awayCmd      = Away <$> (A8.string "away" *> I.space *> remainder)
backCmd      = Back <$ A8.string "back"
debugCmd     = Debug <$ A8.string "debug"
joinCmd      = Join <$> (A8.string "join" *> I.space *> I.channel)
meCmd        = Me <$> (A8.string "me" *> I.space *> remainder)
msgCmd       = Privmsg <$> (A8.string "msg" *> I.space *> (I.nick <|> I.channel)) 
                        <*> (I.space *> remainder)
namesCmd     = Names <$> (A8.string "names" *> I.space *> remainder)
nCmd         = Names <$> (A8.string "n" *> I.space *> remainder)
nickCmd      = Nick <$> (A8.string "nick" *> I.space *> I.nick)
partCmd      = Part <$> (A8.string "part" *> I.space *> remainder)
pongCmd      = Pong <$> (A8.string "pong" *> I.space *> remainder)
quitCmd      = Quit <$> (A8.string "quit" *> I.space *> remainder)
--removeCmd    = Remove <$> (A8.string "remove" *> I.space *> remainder)
whoCmd       = Who  <$> (A8.string "whois" *> I.space *> I.nick)
unknownCmd   = Unknown <$> remainder

remainder :: A.Parser B.ByteString
remainder = A.takeTill A8.isEndOfLine

--timeCmd    = Time    <$> (string "time"    *> optional (space *> undefined))
--pingCmd    = Ping    <$> (string "ping"    *> space *> undefined)

-- upperCaseFirstWord

toMessage :: CtlCommand -> I.Message
toMessage (Away s) = I.Message Nothing I.AWAY [s]
toMessage Back     = I.Message Nothing I.AWAY []
toMessage (Join s) = I.Message Nothing I.JOIN [s]
--toMessage (Me s)   = I.Message Nothing I.JOIN [s]
toMessage (Names s) = I.Message Nothing I.NAMES [s]
toMessage (Nick s) = I.Message Nothing I.NICK [s]
toMessage (Part s) = I.Message Nothing I.PART [s]
toMessage (Pong s) = I.Message Nothing I.PONG [s]
toMessage (Privmsg t s) = I.Message Nothing I.PRIVMSG [t,B.cons 58 s]
toMessage (Quit s) = I.Message Nothing I.QUIT [s]
--toMessage (Remove s) = I.Message Nothing I.REMOVE [s]
toMessage (Who s) = I.Message Nothing I.WHO [s]
toMessage _ = I.Message Nothing I.ERROR ["unknown CtlCommand"]

