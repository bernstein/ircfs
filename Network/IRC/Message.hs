{-# LANGUAGE OverloadedStrings #-}
module Network.IRC.Message
where

import Control.Applicative hiding (many)
import Data.Attoparsec as P
import qualified Data.Attoparsec.Char8 as P8
import Data.Attoparsec.Char8 (char8, endOfLine, isDigit_w8)
import Data.Word (Word8)
import qualified Data.ByteString.Char8 as B hiding (map)
import qualified Data.ByteString as B (map)
import qualified Data.ByteString as BS
import Data.Maybe (maybeToList)

-- RFC 1459
-- 2.3.1
-- max 512 Characters long
-- and rfc2812
-- http://www.irchelp.org/irchelp/rfc/rfc2812.txt

data Message = Message (Maybe Prefix) Command Params
  deriving (Eq, Ord, Show)
--  <message>  ::= [':' <prefix> <SPACE> ] <command> <params> <crlf>
message :: Parser Message
message = 
  Message <$> optional (char8 ':' *> prefix <* space) 
          <*> command
          <*> params
          <*  crlf

data Prefix = PrefixServer !Servername
            | PrefixNick !Nick !(Maybe User) !(Maybe Host)
  deriving (Eq,Ord,Show)
--  <prefix>   ::= <servername> | <nick> [ '!' <user> ] [ '@' <host> ]
prefix :: Parser Prefix
prefix = 
  PrefixNick <$> nick 
             <*> optional (char8 '!' *> user)
             <*> optional (char8 '@' *> host)
  <|> PrefixServer <$> servername
  
data Command = CmdString B.ByteString | CmdNr Int
  deriving (Show,Read,Eq,Ord)
--  <command>  ::= <letter> { <letter> } | <number> <number> <number>
command :: Parser Command
command =  CmdNr <$> threeDigitNumber
       <|> CmdString <$> takeWhile1 isLetter 

--  <SPACE>    ::= ' ' { ' ' }
space :: Parser BS.ByteString
space = takeWhile1 (==32)

skipSpaces :: Parser ()
skipSpaces = satisfy P8.isHorizontalSpace *> skipWhile P8.isHorizontalSpace

--  <params>   ::= <SPACE> [ ':' <trailing> | <middle> <params> ]
type Params = [B.ByteString]
params :: Parser Params
params = (++) <$> many (skipSpaces *> middle)
            <*> (maybeToList <$> optional (char8 ' ' *> char8 ':' *> trailing))
-- i do not consider the alternive
-- exactly 14 (space middle) parts and a (space trailing) part where the colon
-- is optional
-- =/ 14( SPACE middle ) [ SPACE [ ":" ] trailing ]

type Parameter = B.ByteString

--  <middle>   ::= <Any *non-empty* sequence of octets not including SPACE
--                 or NUL or CR or LF, the first of which may not be ':'>
type Middle = B.ByteString
middle :: Parser Middle
middle = BS.cons <$> nospcrlfcl <*> P.takeWhile (\c -> isNospcrlfcl c || c == 58)
--middle = (BS.cons) <$> satisfy first <*> P.takeWhile valid
--    where valid = notInClass "\32\NUL\r\n"
--          first = notInClass ":\32\NUL\r\n"

--  <trailing> ::= <Any, possibly *empty*, sequence of octets not including
--                                  NUL or CR or LF>
type Trailing = B.ByteString
trailing :: Parser Trailing
trailing = BS.pack <$> many (char8 ':' <|> char8 ' ' <|> nospcrlfcl) 

--  <crlf>     ::= CR LF
crlf :: Parser ()
crlf = () <$ (string "\r\n")
--crlf = endOfLine -- (() <$ string "\r\n") <|> (() <$ char '\n')

-- Most protocol messages specify additional semantics and syntax for
-- the extracted parameter strings dictated by their position in the
-- list.  For example, many server commands will assume that the first
-- parameter after the command is the list of targets, which can be
-- described with:

-- <target>     ::= <to> [ "," <target> ]
type Target = [To]
target :: Parser Target
target = to `sepBy1` (char8 ',')

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
channel = (BS.cons) <$> satisfy (inClass "#&") <*> takeWhile1 isChstring

type Servername = Host
-- <servername> ::= <host>
servername :: Parser Servername
servername = host

type Host = B.ByteString
-- <host>       ::= see RFC 952 [DNS:4] for details on allowed hostnames
host :: Parser Host
host = BS.cons <$> letter <*> P.takeWhile (inClass "a-zA-Z0-9.-")

type Nick = B.ByteString
-- <nick>       ::= <letter> { <letter> | <number> | <special> }
nick :: Parser Nick
nick = BS.cons <$> letter <*> P.takeWhile 
                            (\c -> isLetter c || P8.isDigit_w8 c || isSpecial c)

type Mask = B.ByteString
-- <mask>       ::= ('#' | '$') <chstring>^
mask :: Parser Mask
mask = (BS.cons) <$> satisfy (inClass "#$") <*> takeWhile1 isChstring

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

-- <special>    ::= '-' | '[' | ']' | '\' | '`' | '^' | '{' | '}'
special :: Parser Word8
special = satisfy isSpecial

isSpecial :: Word8 -> Bool
isSpecial = (`elem` [45,91,93,96,92,94,123,125])
-- (`elem` (map ord "-[]\\`^{}"))

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
  where addUp a b c = (fromIntegral (a-48) * 100 
                    + fromIntegral (b-48) * 10 
                    + fromIntegral (c-48))

-- nospcrlfcl = %x01-09 / %x0B-0C / %x0E-1F / %x21-39 / %x3B-FF
nospcrlfcl :: Parser Word8
nospcrlfcl = satisfy isNospcrlfcl

isNospcrlfcl :: Word8 -> Bool
isNospcrlfcl = not.(`elem` [0,10,13,32,58])
--isNospcrlfcl = inClass "\SOH-\t\v\f\SO-\US!-9;-\255"
-- nospcrlfclSet =
--   map ord (['\x01'..'\x09'] ++ ['\x0B'..'\x0C'] ++ ['\x0E'..'\x1F'] ++ ['\x21'..'\x39'] ++ ['\x3B'..'\xFF'])
-- [0,10,13,32,58] == [0..255] \\ nospcrlfclSet
-- map chr [0,10,13,32,58] == "\NUL\n\r :"
-- unittest 
-- is_nospcrlfcl_spec = inClass 
--   (['\x01'..'\x09'] ++ ['\x0B'..'\x0C'] ++ ['\x0E'..'\x1F'] ++ ['\x21'..'\x39'] ++ ['\x3B'..'\xFF'])
-- test = all (uncurry (==)) $ zip (map is_nospcrlfcl_spec [0..255]) (map isNospcrlfcl [0..255])

-- prop_isnospcrlfcl c = isNospcrlfcl c == is_nospcrlfcl_spec c
 
testMsg1 = 
  ":CalebDelnay!calebd@localhost PRIVMSG #mychannel :Hello everyone!\r\n"
testMsg2 = ":CalebDelnay!calebd@localhost QUIT :Bye bye!\r\n"
testMsg3 = ":CalebDelnay!calebd@localhost JOIN #mychannel\r\n"
testMsg4 = ":CalebDelnay!calebd@localhost MODE #mychannel -l\r\n"
testMsg5 = "PING :irc.localhost.localdomain\r\n"

