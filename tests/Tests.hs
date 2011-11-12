{-# LANGUAGE OverloadedStrings #-}
module Main
where

import Control.Applicative hiding (many)
import Data.Monoid
import Data.List (foldl')
import Data.Attoparsec
import Data.Attoparsec.Char8 (char8)
import Test.QuickCheck
import qualified Network.IRC.Message as I
import System.Random (Random(..), RandomGen)
import Data.Word (Word8)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B hiding (map)

instance Arbitrary I.Params where
  -- XXX TODO fix mE
  arbitrary = I.Params <$> undefined <*> undefined

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

instance Arbitrary I.Prefix where
  arbitrary = oneof [ I.PrefixServer <$> arbServer
                    , I.PrefixNick <$> arbNick <*> arbMaybeUser <*> arbMaybeHost
                    ]

instance Arbitrary I.Command where
  arbitrary = elements
          [ I.PASS ,I.NICK ,I.USER ,I.OPER ,I.MODE ,I.SERVICE ,I.QUIT ,I.SQUIT 
          , I.JOIN ,I.PART, I.NAMES ,I.KICK ,I.PRIVMSG ,I.NOTICE ,I.MOTD
          , I.TIME ,I.WHO ,I.PING ,I.AWAY ,I.TOPIC ,I.PONG ,I.INVITE ,I.WHOIS 
          ,I.ERROR ]

instance Arbitrary B.ByteString where
  arbitrary = B.pack <$> arbitrary

instance Arbitrary I.Message where
  arbitrary = I.Message <$> arbitrary <*> arbitrary <*> arbitrary

prop_message :: I.Message -> Bool
prop_message m = 
  Just m == maybeResult ( parse I.message (I.toByteString m))

prop_command :: I.Command -> Bool
prop_command c = 
  Just c == maybeResult (feed (parse I.command (B.pack (show c))) B.empty)

prop_prefix :: I.Prefix -> Bool
prop_prefix c = 
  Just c == maybeResult (feed (parse (char8 ':' *> I.prefix) (I.prefixToByteString c `B.append` " ")) B.empty)

prop_params :: I.Params -> Bool
prop_params ps =
  Just ps == maybeResult (feed (parse I.params (I.paramsToByteString ps)) mempty)

main :: IO ()
main =
  quickCheckWith (stdArgs { maxSuccess = 10000, maxSize = 200 }) $
      (label "prop_message" prop_message)
  .&. (label "prop_prefix" prop_prefix)
  .&. (label "prop_command" prop_command)
  .&. (label "prop_params" prop_params)

