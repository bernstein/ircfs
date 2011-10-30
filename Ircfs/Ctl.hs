{-# LANGUAGE OverloadedStrings #-}
module Ircfs.Ctl
where

import Control.Applicative hiding (many)
import qualified Data.ByteString as B
import qualified Data.Attoparsec as A
import qualified Data.Attoparsec.Char8 as A8
import qualified Network.IRC.Message as I

data CtlCommand =
    Away { m :: B.ByteString }
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
  | Names { m :: B.ByteString }
  | Nick { nickname :: B.ByteString }
  | Notice { msgtarget :: B.ByteString, msg :: B.ByteString }
  | Op
  | Part { msg :: B.ByteString }
  | Ping { msg :: B.ByteString } 
  | Privmsg { msgtarget :: B.ByteString, msg :: B.ByteString }
  | Quit { m :: B.ByteString }
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
      <|> namesCmd
      <|> nCmd
      <|> nickCmd
      <|> partCmd
      <|> msgCmd
      <|> whoCmd
      <|> unknownCmd

awayCmd      = Away <$> (A8.string "away" *> I.space *> remainder)
backCmd      = Back <$ A8.string "back"
debugCmd     = Debug <$ A8.string "debug"
joinCmd      = Join <$> (A8.string "join" *> I.space *> I.channel)
meCmd        = Me <$> (A8.string "me" *> I.space *> remainder)
namesCmd     = Names <$> (A8.string "names" *> I.space *> remainder)
nCmd         = Names <$> (A8.string "n" *> I.space *> remainder)
nickCmd      = Nick <$> (A8.string "nick" *> I.space *> I.nick)
partCmd      = Part <$> (A8.string "part" *> I.space *> remainder)
msgCmd       = Privmsg <$> (A8.string "msg" *> I.space *> (I.nick <|> I.channel)) 
                        <*> (I.space *> remainder)
removeCmd    = Remove <$> (A8.string "remove" *> I.space *> remainder)
whoCmd       = Who  <$> (A8.string "whois" *> I.space *> I.nick)
unknownCmd   = Unknown <$> remainder

remainder :: A.Parser B.ByteString
remainder = A.takeTill A8.isEndOfLine

--timeCmd    = Time    <$> (string "time"    *> optional (space *> undefined))
--pingCmd    = Ping    <$> (string "ping"    *> space *> undefined)

-- upperCaseFirstWord

toMessage :: CtlCommand -> I.Message
toMessage Back = I.Message Nothing I.AWAY []
toMessage (Nick s) = I.Message Nothing I.NICK [s]
toMessage (Join s) = I.Message Nothing I.JOIN [s]
toMessage (Part s) = I.Message Nothing I.PART [s]
toMessage (Privmsg t s) = I.Message Nothing I.PRIVMSG [t,B.cons 58 s]
toMessage _ = error "toMessage: implement the rest of me"

