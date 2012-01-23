{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Network.IRC.Message.Parser
-- Copyright   :  (c) Andreas-Christoph Bernstein 2011
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  andreas.bernstein@googlemail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Parse an IRC message string. The string must end with '\r\n'.

module Network.IRC.Message.Parser
  (
    message
  , space
  , command
  , params
  , channel
  , prefix
  , nick
  ) where

import           Prelude hiding (takeWhile)
import           Control.Applicative -- hiding (many)
import           Data.Attoparsec
import qualified Data.Attoparsec.Char8 as P8
import           Data.Attoparsec.Char8 (char8)
import Data.Word (Word8)
import qualified Data.ByteString.Char8 as B hiding (map)
import qualified Data.ByteString as BS
import           Data.Monoid
import           Network.IRC.Message.Types

-- RFC 1459
-- 2.3.1
-- max 512 Characters long
-- and rfc2812
-- http://www.irchelp.org/irchelp/rfc/rfc2812.txt

message :: Parser Message
message =
  --Message <$> optional (char8 ':' *> prefix <* space) 
  Message <$> optional (char8 ':' *> prefix)
          <*> command
          <*> params
          <*  crlf

prefix :: Parser Prefix
prefix = prefixNick <|> prefixServer
  where prefixServer = PrefixServer <$> servername <* space
        prefixNick = PrefixNick <$> nick
                    <*> optional (char8 '!' *> user)
                    <*> optional (char8 '@' *> host)
                    <* space -- lookAhead (char8 ' ')

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

space :: Parser BS.ByteString
space = takeWhile1 (==32)

params :: Parser Params
params = 
  let m = many (char8 ' ' *> middle)
      t = optional (char8 ' ' *> char8 ':' *> trailing)
  in Params <$> m <*> t
-- i do not consider the alternive
-- exactly 14 (space middle) parts and a (space trailing) part where the colon
-- is optional
-- =/ 14( SPACE middle ) [ SPACE [ ":" ] trailing ]

type Middle = B.ByteString
middle :: Parser Middle
middle = BS.cons <$> nospcrlfcl <*> takeWhile (\c -> isNospcrlfcl c || c == 58)

type Trailing = B.ByteString
trailing :: Parser Trailing
trailing = BS.pack <$> many (char8 ':' <|> char8 ' ' <|> nospcrlfcl)

nocrlfcl :: Parser Word8
nocrlfcl = satisfy isNospcrlfcl

isNocrlfcl :: Word8 -> Bool
isNocrlfcl = not.(`elem` [0,10,13])

crlf :: Parser ()
crlf = () <$ string "\r\n"

type Target = [To]
target :: Parser Target
target = to `sepBy1` char8 ','

data To = ToChannel Channel
        | ToUserAtServer User Servername
        | ToNick Nick
        | ToMask Mask

to :: Parser To
to =  ToChannel <$> channel
  <|> ToUserAtServer <$> (user <* char8 '@') <*> servername
  <|> ToNick <$> nick
  <|> ToMask <$> mask

type Channel = B.ByteString

channel :: Parser Channel
channel = BS.cons <$> satisfy (inClass "#&") <*> takeWhile1 isChstring

type Servername = Host
servername :: Parser Servername
servername = host

type Host = B.ByteString
host :: Parser Host
host = takeWhile1 (inClass "a-zA-Z0-9./-")

type Nick = B.ByteString
nick :: Parser Nick
nick = BS.cons <$> letter <*> takeWhile 
                            (\c -> isLetter c || P8.isDigit_w8 c || isSpecial c)

type Mask = B.ByteString
mask :: Parser Mask
mask = BS.cons <$> satisfy (inClass "#$") <*> takeWhile1 isChstring

chstring :: Parser Word8
chstring = satisfy isChstring

isChstring :: Word8 -> Bool
isChstring = notInClass "\32\BEL\NUL\r\n,"

type User = B.ByteString
user :: Parser User
user = takeWhile1 (notInClass "\0\13\10\32@")

letter :: Parser Word8 -- Char
letter = satisfy isLetter

isLetter :: Word8 -> Bool
isLetter = inClass "a-zA-Z"

number :: Parser Word8
number = digit

digit :: Parser Word8
digit = satisfy P8.isDigit_w8

special :: Parser Word8
special = satisfy isSpecial

isSpecial :: Word8 -> Bool
isSpecial = (`elem` [45,91,93,96,92,94,95,123,124,125])

nonwhite :: Parser Word8
nonwhite = satisfy isNonWhite

isNonWhite :: Word8 -> Bool
isNonWhite = not.(`elem` [32,00,13,10])

threeDigitNumber :: Parser Int
threeDigitNumber = addUp <$> number <*> number <*> number 
  where addUp a b c = fromIntegral (a-48) * 100 
                    + fromIntegral (b-48) * 10
                    + fromIntegral (c-48)

nospcrlfcl :: Parser Word8
nospcrlfcl = satisfy isNospcrlfcl

-- nospcrlfcl =  %x01-09 / %x0B-0C / %x0E-1F / %x21-39 / %x3B-FF
isNospcrlfcl :: Word8 -> Bool
isNospcrlfcl = not.(`elem` [0,10,13,32,58])

