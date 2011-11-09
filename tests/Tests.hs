{-# LANGUAGE OverloadedStrings #-}
module Main
where

import Control.Applicative hiding (many)
import Data.Monoid
import Data.List (foldl')
import Data.Attoparsec
import           Data.Attoparsec.Char8 (char8)
import Test.QuickCheck
import Network.IRC.Message
import System.Random (Random(..), RandomGen)
import Data.Word (Word8)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B hiding (map)

newtype Params = Params { unParams :: [B.ByteString] }
  deriving (Show, Eq, Ord)

instance Arbitrary Params where
  -- XXX TODO fix mE
  arbitrary = Params <$> listOf1 arbNick

arbNick :: Gen B.ByteString
arbNick = BS.cons <$> letter <*> lnss

arbHost :: Gen B.ByteString
arbHost = foldr BS.cons mempty <$> xs
  where c :: Gen Word8 -- "a-z"        "A-Z"        "-./0-9"
        c = elements $ [97..122] ++ [65..90] ++ [45,48..57]
        xs = do
               a <- listOf1 c 
               b <- listOf1 c
               let dot = [46] -- at least one '.'
               return $ a ++ dot ++ b 

arbServer :: Gen B.ByteString
arbServer = arbHost

arbMaybeUser :: Gen (Maybe B.ByteString)
arbMaybeUser = frequency [(1, pure Nothing), (2, Just <$> arbNick)]

arbMaybeHost :: Gen (Maybe B.ByteString)
arbMaybeHost = frequency [(1, pure Nothing), (2, Just <$> arbHost)]

lnss :: Gen B.ByteString
lnss = foldr BS.cons mempty <$> listOf lns

instance Random Word8 where
  randomR = integralRandomR
  random = randomR (minBound, maxBound)

integralRandomR (a,b) g = case randomR (c,d) g of
                            (x,h) -> (fromIntegral x, h)
  where (c,d) = (fromIntegral a :: Integer
                ,fromIntegral b :: Integer)

letter :: Gen Word8
letter = frequency [(1, choose (65,90)), (1, choose (97,122)) ]

digit :: Gen Word8
digit = choose (48, 57)

number :: Gen Word8
number = digit

special :: Gen Word8
special = elements [45,91,93,96,92,94,95,123,124,125]

-- letter | number | special
lns :: Gen Word8
lns = frequency [(1, letter), (1, number), (1, special)]

instance Arbitrary Prefix where
  arbitrary = oneof [ PrefixServer <$> arbServer
                    , PrefixNick <$> arbNick <*> arbMaybeUser <*> arbMaybeHost
                    ]

instance Arbitrary Command where
  arbitrary = oneof . map pure $
              [PASS ,NICK ,USER ,OPER ,MODE ,SERVICE ,QUIT ,SQUIT ,JOIN ,PART 
              ,NAMES ,KICK ,PRIVMSG ,NOTICE ,MOTD ,TIME ,WHO ,PING ,AWAY 
              ,TOPIC ,PONG ,INVITE ,WHOIS ,ERROR ]

instance Arbitrary B.ByteString where
  arbitrary = B.pack <$> arbitrary

instance Arbitrary Message where
  arbitrary = Message <$> arbitrary <*> arbitrary <*> ps
    where ps = filter (not . B.null) . unParams <$> arbitrary

prop_message :: Message -> Bool
prop_message m = 
  Just m == maybeResult ( parse message (toByteString m))

prop_command :: Command -> Bool
prop_command c = 
  Just c == maybeResult (feed (parse command (B.pack (show c))) B.empty)

prop_prefix :: Prefix -> Bool
prop_prefix c = 
  Just c == maybeResult (feed (parse (char8 ':' *> prefix) (prefixToByteString c `B.append` " ")) B.empty)

prop_params :: Params -> Bool
prop_params ps =
  Just p == maybeResult (feed (parse params (paramsToByteString p)) B.empty)
    where p = unParams ps

main :: IO ()
main =
  quickCheckWith (stdArgs { maxSuccess = 1000, maxSize = 200 }) $
      (label "prop_message" prop_message)
  .&. (label "prop_prefix" prop_prefix)
  .&. (label "prop_command" prop_command)
  .&. (label "prop_params" prop_params)

