{-# LANGUAGE OverloadedStrings #-}
module Main
where

import           Prelude hiding ((.), id)
import           Control.Category
import           Data.Monoid
import           Data.Maybe
import           Control.Applicative
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BS
import qualified Network.IRC.Message as I
import qualified Data.Attoparsec as A
import           Data.Word (Word8)
import           System.Random (RandomGen, Random(..))
import           Data.Attoparsec.Char8 (char8)
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck hiding (property)

-- message    =  [ ":" prefix SPACE ] command [ params ] crlf
genMessage :: Gen B.ByteString
                          -- promote
genMessage = BS.concat <$> sequence [pre , genCommandStr, ps, pure "\r\n"]
  where pre = oneof [ pure mempty
                    , B.append ":" <$> (B.append <$> genPrefix <*> pure " ")]
        ps = oneof [ pure mempty, genParamsStr]
 
-- prefix     =  servername / ( nickname [ [ "!" user ] "@" host ] )
genPrefix :: Gen B.ByteString
genPrefix = frequency [(1,genServername), (3, genNickPrefix)]
  where genNickPrefix = B.append <$> genNickname <*> genH
        genH = oneof [ pure mempty
                     , B.append <$> genU <*> (B.cons 64 <$> genServername)]
        genU = oneof [ pure mempty, B.append "!" <$> genUser] 

-- command    =  1*letter / 3digit
-- params     =  *14( SPACE middle ) [ SPACE ":" trailing ]
--            =/ 14( SPACE middle ) [ SPACE [ ":" ] trailing ]
genParamsStr :: Gen B.ByteString
genParamsStr = B.append <$> genHead <*> genTail
  where
    genHead = B.append " " <$> genMiddle
    genTail = oneof [pure mempty, B.append <$> (pure " :") <*> genTrailing]

-- nospcrlfcl =  %x01-09 / %x0B-0C / %x0E-1F / %x21-39 / %x3B-FF
genNospcrlfcl :: Gen Word8
genNospcrlfcl = elements $
  [0x01..0x09]++[0x0B..0x0C]++[0x0E..0x1F]++[0x21..0x39]++[0x3B..0xFF]

-- middle     =  nospcrlfcl *( ":" / nospcrlfcl )
genMiddle :: Gen B.ByteString
genMiddle = B.pack <$> ( (:) <$> 
    genNospcrlfcl <*> listOf (frequency [(1, pure 58), (9, genNospcrlfcl)])) 

-- trailing   =  *( ":" / " " / nospcrlfcl )
genTrailing :: Gen B.ByteString
genTrailing = B.pack <$> 
  listOf (frequency [(1,pure 58), (3,pure 32), (9, genNospcrlfcl)])

type Servername = Hostname

genServername :: Gen B.ByteString
genServername = genHostname

-- host       =  hostname / hostaddr
newtype Host = Host { unHost :: B.ByteString } deriving (Show, Eq)
instance Arbitrary Host where
  arbitrary = undefined

-- hostname   =  shortname *( "." shortname )
newtype Hostname = Hostname { unHostname :: B.ByteString } deriving (Show, Eq)
instance Arbitrary Hostname where
  arbitrary = Hostname <$> genHostname

genHostname = B.append <$> 
      genShortname <*> (B.concat <$> listOf g)
    where   g :: Gen B.ByteString
            g = B.cons 0x2E <$> genShortname

--  shortname  =  ( letter / digit ) *( letter / digit / "-" )
--                *( letter / digit )
--                  ; as specified in RFC 1123 [HNAME]
newtype Shortname = Shortname { unShortname :: B.ByteString } 
  deriving (Show, Eq)
instance Arbitrary Shortname where
  arbitrary = Shortname <$> genShortname

genShortname :: Gen B.ByteString
genShortname = B.pack <$> ((:) <$> oneof [genLetter,genDigit] <*> listOf (oneof [genLetter, genDigit, pure 0x2D]))

-- hostaddr   =  ip4addr / ip6addr
newtype Hostaddr = Hostaddr { unHostaddr :: B.ByteString } deriving (Show, Eq)
instance Arbitrary Hostaddr where
  arbitrary = undefined

-- ip4addr    =  1*3digit "." 1*3digit "." 1*3digit "." 1*3digit
newtype Ip4addr = Ip4addr { unIp4addr :: B.ByteString } deriving (Show, Eq)
instance Arbitrary Ip4addr where
  arbitrary = undefined

-- ip6addr    =  1*hexdigit 7( ":" 1*hexdigit )
newtype Ip6addr = Ip6addr { unIp6addr :: B.ByteString } deriving (Show, Eq)
instance Arbitrary Ip6addr where
  arbitrary = undefined
-- ip6addr    =/ "0:0:0:0:0:" ( "0" / "FFFF" ) ":" ip4addr

-- nickname   =  ( letter / special ) *8( letter / digit / special / "-" )
newtype Nickname = Nickname { unNickname :: B.ByteString } deriving (Show, Eq)
instance Arbitrary Nickname where
  arbitrary = Nickname <$> genNickname

-- old RFC says
-- <nick>       ::= <letter> { <letter> | <number> | <special> }
genNickname :: Gen B.ByteString
--genNickname = B.pack <$> ((:) <$> oneof [genLetter,genSpecial] <*> listOf (oneof [genLetter, genDigit, genSpecial, pure 0x2D]))
genNickname = B.pack <$> ((:) <$> genLetter <*> listOf (oneof [genLetter, genDigit, genSpecial, pure 0x2D]))

newtype PrefixString = PrefixString { unPrefixString :: B.ByteString }
  deriving (Show, Eq, Ord)

instance Arbitrary PrefixString where
  arbitrary = PrefixString <$> genPrefix

newtype CommandString = CommandString { unCommandString :: B.ByteString }
  deriving (Show, Eq, Ord)

instance Arbitrary CommandString where
  arbitrary = CommandString <$> genCommandStr

genCommandStr :: Gen B.ByteString
genCommandStr = oneof [getCommandStr, gen3DigitNrStr]

instance Arbitrary B.ByteString where
  arbitrary = B.pack <$> arbitrary

-- user       =  1*( %x01-09 / %x0B-0C / %x0E-1F / %x21-3F / %x41-FF )
--                 ; any octet except NUL, CR, LF, " " and "@"
genUser :: Gen B.ByteString
genUser = B.pack <$> listOf1 g
  where g :: Gen Word8
        g = elements $
          [0x01..0x09]++[0x0B..0x0C]++[0x0E..0x1F]++[0x21..0x3F]++[0x41..0xFF]

-- letter     =  %x41-5A / %x61-7A       ; A-Z / a-z
genLetter :: Gen Word8
genLetter = elements $ [0x41..0x5A] ++ [0x61..0x7A]

-- special    =  %x5B-60 / %x7B-7D
--                 ; "[", "]", "\", "`", "_", "^", "{", "|", "}"
genSpecial :: Gen Word8
genSpecial = elements $ [0x5B..0x60] ++ [0x7B..0x7D]

-- digit      =  %x30-39                 ; 0-9
genDigit :: Gen Word8
genDigit = elements $ [0x30..0x39]

prop_equal_prefix :: PrefixString -> Bool
prop_equal_prefix pr = Just str == f str
  where f = fmap I.prefixToByteString . mP I.prefix 
        str  = unPrefixString pr
mP p s = A.maybeResult $ A.feed (A.feed (A.parse p s) " ") mempty

prop_equal_command :: CommandString -> Bool
prop_equal_command cmd = Just s == f s
  where f = fmap I.commandToByteString . maybeP I.command 
        s  = unCommandString cmd

prop_idempotent_params :: Property
prop_idempotent_params = forAll genParamsStr (\ps -> (e =<< e ps) == e ps)
  where e = fmap I.paramsToByteString . maybeP I.params

prop_idempotent_message :: Property
prop_idempotent_message = forAll genMessage (\ms -> (e =<< e ms) == e ms)
  where e = fmap I.toByteString . maybeP I.message

getCommandStr :: Gen B.ByteString
getCommandStr = elements $
    [ "ADMIN", "AWAY", "CONNECT", "DIE", "ERROR", "INFO", "INVITE", "ISON"
    , "JOIN", "KICK", "KILL", "LINKS", "LIST", "LUSERS", "MODE", "MOTD"
    , "NAMES", "NICK", "NOTICE", "OPER", "PART", "PASS", "PING", "PONG"
    , "PRIVMSG", "QUIT", "REHASH", "RESTART", "SERVICE", "SERVLIST", "SQUERY"
    , "SQUIT", "STATS", "SUMMON", "TIME", "TOPIC", "TRACE", "USER", "USERHOST"
    , "USERS", "VERSION", "WALLOPS", "WHO", "WHOIS", "WHOWAS"
    ]

gen3DigitNrStr :: Gen B.ByteString
gen3DigitNrStr = elements . map BS.pack $
              [concatMap show [x,y,z] | x <- [0..9], y <- [0..9], z <- [0..9]]

specs :: Specs
specs = describe "Message" [
  it "command" 
    (property prop_equal_command)
  ]

maybeP ::  A.Parser r -> BS.ByteString -> Maybe r
maybeP p s = A.maybeResult $ A.feed (A.parse p s) mempty

successfullyParsed p = isJust . maybeP p

prop_parsed = forAll genMessage (successfullyParsed I.message)

{-
idempotent :: Eq a => (a -> a) -> a -> Bool
idempotent e a = e (e a) == e a
-}

-- I.prefix expects an whitespace after the prefix !!!

main :: IO ()
main = do
  -- hspec specs
  quickCheckWith stdArgs { maxSuccess = 10000, maxSize = 300 } (
        (label "prop_equal_command" prop_equal_command)
    .&. (label "prop_equal_prefix" prop_equal_prefix)
    .&. (label "prop_parsed" prop_parsed)
    .&. (label "prop_idempotent_message" prop_idempotent_message)
    .&. (label "prop_idempotent_params" prop_idempotent_params))

