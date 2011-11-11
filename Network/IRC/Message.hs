{-# LANGUAGE OverloadedStrings #-}
-- -----------------------------------------------------------------------------
-- |
-- Module      :  Network.IRC.Message
-- Copyright   :  (c) Andreas-Christoph Bernstein 2011
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  andreas.bernstein@googlemail.com
-- Stability   :  unstable
-- Portability :  not portable
--
-- 
--
--------------------------------------------------------------------------------

module Network.IRC.Message
  (
    Message(..)
  , message
  , space
  , command
  , params
  , channel
  , prefix
  , nick
  , Command(..)
  , Prefix(..)
  , Params(..)
  , toByteString 
  , prefixToByteString
  , commandToByteString
  , paramsToByteString
  ) where

import           Prelude hiding (takeWhile)
import           Control.Applicative hiding (many)
import           Data.Attoparsec
import qualified Data.Attoparsec.Char8 as P8
import           Data.Attoparsec.Char8 (char8)
import Data.Word (Word8)
import qualified Data.ByteString.Char8 as B hiding (map)
import qualified Data.ByteString as BS
import           Data.Maybe (maybeToList)
import           Data.Monoid

-- RFC 1459
-- 2.3.1
-- max 512 Characters long
-- and rfc2812
-- http://www.irchelp.org/irchelp/rfc/rfc2812.txt

data Message = Message !(Maybe Prefix) !Command !Params
  deriving (Eq, Ord, Show)
--  <message>  ::= [':' <prefix> <SPACE> ] <command> <params> <crlf>
message :: Parser Message
message =
  --Message <$> optional (char8 ':' *> prefix <* space) 
  Message <$> optional (char8 ':' *> prefix)
          <*> command
          <*> params
          <*  crlf

data Prefix = PrefixServer !Servername
            | PrefixNick !Nick !(Maybe User) !(Maybe Host)
  deriving (Eq,Ord,Show)
--  <prefix>   ::= <servername> | <nick> [ '!' <user> ] [ '@' <host> ]
prefix :: Parser Prefix
prefix = prefixNick <|> prefixServer
  where prefixServer = PrefixServer <$> servername <* space
        prefixNick = PrefixNick <$> nick
                    <*> optional (char8 '!' *> user)
                    <*> optional (char8 '@' *> host)
                    <* space -- lookAhead (char8 ' ')

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
  | CmdString B.ByteString
  deriving (Show,Read,Eq,Ord)
--  <command>  ::= <letter> { <letter> } | <number> <number> <number>
command :: Parser Command
command =  CmdNumericReply   <$> threeDigitNumber
       <|> ADMIN <$ string "ADMIN"
       <|> AWAY <$ string "AWAY"
       <|> CONNECT <$ string "CONNECT"
       <|> DIE <$ string "DIE"
       <|> ERROR <$ string "ERROR"
       <|> INFO <$ string "INFO"
       <|> INVITE <$ string "INVITE"
       <|> ISON <$ string "ISON"
       <|> JOIN <$ string "JOIN"
       <|> KICK <$ string "KICK"
       <|> KILL <$ string "KILL"
       <|> LINKS <$ string "LINKS"
       <|> LIST <$ string "LIST"
       <|> LUSERS <$ string "LUSERS"
       <|> MODE <$ string "MODE"
       <|> MOTD <$ string "MOTD"
       <|> NAMES <$ string "NAMES"
       <|> NICK <$ string "NICK"
       <|> NOTICE <$ string "NOTICE"
       <|> OPER <$ string "OPER"
       <|> PART <$ string "PART"
       <|> PASS <$ string "PASS"
       <|> PING <$ string "PING"
       <|> PONG <$ string "PONG"
       <|> PRIVMSG <$ string "PRIVMSG"
       <|> QUIT <$ string "QUIT"
       <|> REHASH <$ string "REHASH"
       <|> RESTART <$ string "RESTART"
       <|> SERVICE <$ string "SERVICE"
       <|> SERVLIST <$ string "SERVLIST"
       <|> SQUERY <$ string "SQUERY"
       <|> SQUIT <$ string "SQUIT"
       <|> STATS <$ string "STATS"
       <|> SUMMON <$ string "SUMMON"
       <|> TIME <$ string "TIME"
       <|> TOPIC <$ string "TOPIC"
       <|> TRACE <$ string "TRACE"
       <|> USERHOST <$ string "USERHOST"
       <|> USERS <$ string "USERS"
       <|> USER <$ string "USER"
       <|> VERSION <$ string "VERSION"
       <|> WALLOPS <$ string "WALLOPS"
       <|> WHOIS <$ string "WHOIS"
       <|> WHOWAS <$ string "WHOWAS"
       <|> WHO <$ string "WHO"
       <|> ((CmdString <$> takeWhile1 isLetter ) <?> "Cmdstring hat nen problem")
       -- <|> fail "command"

--  <SPACE>    ::= ' ' { ' ' }
space :: Parser BS.ByteString
space = takeWhile1 (==32)

skipSpaces :: Parser ()
skipSpaces = satisfy P8.isHorizontalSpace *> skipWhile P8.isHorizontalSpace

--  <params>   ::= <SPACE> [ ':' <trailing> | <middle> <params> ]
data Params = Params [B.ByteString] (Maybe B.ByteString)
  deriving (Eq,Ord,Show,Read)

params :: Parser Params
params = 
  let m = many (skipSpaces *> middle)
      t = optional (char8 ' ' *> char8 ':' *> trailing)
  in Params <$> m <*> t
-- i do not consider the alternive
-- exactly 14 (space middle) parts and a (space trailing) part where the colon
-- is optional
-- =/ 14( SPACE middle ) [ SPACE [ ":" ] trailing ]

type Parameter = B.ByteString

--  <middle>   ::= <Any *non-empty* sequence of octets not including SPACE
--                 or NUL or CR or LF, the first of which may not be ':'>
type Middle = B.ByteString
middle :: Parser Middle
middle = BS.cons <$> nospcrlfcl <*> takeWhile (\c -> isNospcrlfcl c || c == 58)
--middle = (BS.cons) <$> satisfy first <*> P.takeWhile valid
--    where valid = notInClass "\32\NUL\r\n"
--          first = notInClass ":\32\NUL\r\n"

--  <trailing> ::= <Any, possibly *empty*, sequence of octets not including
--                                  NUL or CR or LF>
type Trailing = B.ByteString
trailing :: Parser Trailing
trailing = BS.pack <$> many (char8 ':' <|> char8 ' ' <|> nospcrlfcl)

-- TODO XXX: nospcrlfcl or nocrlfcl ???
nocrlfcl :: Parser Word8
nocrlfcl = satisfy isNospcrlfcl

isNocrlfcl :: Word8 -> Bool
isNocrlfcl = not.(`elem` [0,10,13])

--  <crlf>     ::= CR LF
crlf :: Parser ()
crlf = () <$ string "\r\n"
--crlf = endOfLine -- (() <$ string "\r\n") <|> (() <$ char '\n')

-- Most protocol messages specify additional semantics and syntax for
-- the extracted parameter strings dictated by their position in the
-- list.  For example, many server commands will assume that the first
-- parameter after the command is the list of targets, which can be
-- described with:

-- <target>     ::= <to> [ "," <target> ]
type Target = [To]
target :: Parser Target
target = to `sepBy1` char8 ','

data To = ToChannel Channel
        | ToUserAtServer User Servername
        | ToNick Nick
        | ToMask Mask
-- <to>         ::= <channel> | <user> '@' <servername> | <nick> | <mask>
to :: Parser To
to =  ToChannel <$> channel
  <|> ToUserAtServer <$> (user <* char8 '@') <*> servername
  <|> ToNick <$> nick
  <|> ToMask <$> mask

type Channel = B.ByteString
-- <channel>    ::= ('#' | '&') <chstring>
channel :: Parser Channel
channel = BS.cons <$> satisfy (inClass "#&") <*> takeWhile1 isChstring

type Servername = Host
-- <servername> ::= <host>
servername :: Parser Servername
servername = host

type Host = B.ByteString
-- <host>       ::= see RFC 952 [DNS:4] for details on allowed hostnames
host :: Parser Host
-- hostname <|> hostaddr
--host = BS.cons <$> letter <*> takeWhile (inClass "a-zA-Z0-9.-")
host = takeWhile1 (inClass "a-zA-Z0-9./-")

type Nick = B.ByteString
-- <nick>       ::= <letter> { <letter> | <number> | <special> }
nick :: Parser Nick
nick = BS.cons <$> letter <*> takeWhile 
                            (\c -> isLetter c || P8.isDigit_w8 c || isSpecial c)

type Mask = B.ByteString
-- <mask>       ::= ('#' | '$') <chstring>^
mask :: Parser Mask
mask = BS.cons <$> satisfy (inClass "#$") <*> takeWhile1 isChstring

-- <chstring>   ::= <any 8bit code except SPACE, BELL, NUL, CR, LF and
--                      comma (',')>
-- Nul  | 0x00  | 00 | \NUL
-- BELL | 0x07  |  7 | \BEL
-- LF   | 0x0A  | 10 | \n
-- CR   | 0x0D  | 13 | \r
-- SPACE| 0x20  | 32 | ' '
--      | 0x2a  | 44 | ','
chstring :: Parser Word8
chstring = satisfy isChstring

isChstring :: Word8 -> Bool
isChstring = notInClass "\32\BEL\NUL\r\n,"

-- Other parameter syntaxes are:
type User = B.ByteString
-- <user>       ::= <nonwhite> { <nonwhite> }
user :: Parser User
user = takeWhile1 (notInClass "\0\13\10\32@")

-- <letter>     ::= 'a' ... 'z' | 'A' ... 'Z'
letter :: Parser Word8 -- Char
letter = satisfy isLetter

isLetter :: Word8 -> Bool
isLetter = inClass "a-zA-Z"

-- <number>     ::= '0' ... '9'
number :: Parser Word8
number = digit

digit :: Parser Word8
digit = satisfy P8.isDigit_w8

-- <special>  ::= '[' | ']' | '\' | '`' | '_' | '^' | '{' | '|' | '}'
special :: Parser Word8
special = satisfy isSpecial

isSpecial :: Word8 -> Bool
isSpecial = (`elem` [45,91,93,96,92,94,95,123,124,125])
-- (`elem` (map ord "-[]\\`_^{|}"))

-- <nonwhite>   ::= <any 8bit code except SPACE (0x20), NUL (0x0), CR
--                      (0xd), and LF (0xa)>
-- SPACE| 0x20 | 32 | ' '
-- Nul  | 0x0  | 00 | \NUL
-- CR   | 0xd  | 13 | \r
-- LF   | 0xa  | 10 | \n
nonwhite :: Parser Word8
nonwhite = satisfy isNonWhite

isNonWhite :: Word8 -> Bool
isNonWhite = not.(`elem` [32,00,13,10])

threeDigitNumber :: Parser Int
threeDigitNumber = addUp <$> number <*> number <*> number 
  where addUp a b c = fromIntegral (a-48) * 100 
                    + fromIntegral (b-48) * 10
                    + fromIntegral (c-48)

-- nospcrlfcl = %x01-09 / %x0B-0C / %x0E-1F / %x21-39 / %x3B-FF
nospcrlfcl :: Parser Word8
nospcrlfcl = satisfy isNospcrlfcl

isNospcrlfcl :: Word8 -> Bool
isNospcrlfcl = not.(`elem` [0,10,13,32,58])

toByteString :: Message -> B.ByteString
--toByteString (Message Nothing (CmdNumericReply _) ps) = "toByteString: CmdNumericReply not implemented yet"
toByteString (Message Nothing (CmdString _) ps) = "toByteString: CmdString not Implemented yet"
toByteString (Message Nothing cmd ps) = 
  commandToByteString cmd `B.append` paramsToByteString ps `B.append` "\r\n"
toByteString (Message (Just p) cmd ps) =
  ":" 
  `B.append` prefixToByteString p
  `B.append` " "
  `B.append` commandToByteString cmd
  `B.append` paramsToByteString ps
  `B.append` "\r\n"

commandToByteString :: Command -> B.ByteString
commandToByteString (CmdNumericReply x)
  | x < 10 = "00" `B.append` B.pack (show x)
  | x < 100 = "0" `B.append` B.pack (show x)
  | otherwise = B.pack (show x)
commandToByteString c  = B.pack . show $ c

-- also appends a white space to be symmetric to the prefix parser
prefixToByteString :: Prefix -> B.ByteString
prefixToByteString (PrefixServer s) = s
prefixToByteString (PrefixNick n Nothing Nothing) = n
prefixToByteString (PrefixNick n (Just u) Nothing) =
  n `B.append` "!" `B.append` u
prefixToByteString (PrefixNick n Nothing (Just h)) =
  n `B.append` "@" `B.append` h
prefixToByteString (PrefixNick n (Just u) (Just h)) =
  n `B.append` "!" `B.append` u `B.append` "@" `B.append` h

paramsToByteString :: Params -> B.ByteString
paramsToByteString (Params [] Nothing) = mempty
paramsToByteString (Params m@(x:_) Nothing) = " " `B.append` B.unwords m
paramsToByteString (Params [] (Just t)) = " :" `B.append` t
paramsToByteString (Params m@(x:_) (Just t)) = " " `B.append` B.unwords m
                                                `B.append` " :" `B.append` t
