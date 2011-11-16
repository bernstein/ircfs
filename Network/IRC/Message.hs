{-# LANGUAGE OverloadedStrings #-}
-- -----------------------------------------------------------------------------
-- |
-- Module      :  Network.IRC.Message
-- Copyright   :  (c) Andreas-Christoph Bernstein 2011
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  andreas.bernstein@googlemail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Types and functions for working with irc messages.
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
  , encode
  ) where

import           Prelude hiding (takeWhile)
import           Control.Applicative hiding (many)
import           Data.Attoparsec
import qualified Data.Attoparsec.Char8 as P8
import           Data.Attoparsec.Char8 (char8)
import Data.Word (Word8)
import qualified Data.ByteString.Char8 as B hiding (map)
import qualified Data.ByteString as BS
import           Data.Monoid
import           Network.IRC.Message.Types
import           Network.IRC.Message.Parser
import           Network.IRC.Message.Encode

toByteString :: Message -> B.ByteString
toByteString (Message Nothing cmd ps) = 
  commandToByteString cmd `mappend` paramsToByteString ps `mappend` "\r\n"
toByteString (Message (Just p) cmd ps) = mconcat [
    ":" 
  , prefixToByteString p
  , " "
  , commandToByteString cmd
  , paramsToByteString ps
  , "\r\n"]

commandToByteString :: Command -> B.ByteString
commandToByteString (CmdNumericReply x)
  | x < 10 = "00" `mappend` B.pack (show x)
  | x < 100 = "0" `mappend` B.pack (show x)
  | otherwise = B.pack (show x)
commandToByteString c  = B.pack . show $ c

-- also appends a white space to be symmetric to the prefix parser
prefixToByteString :: Prefix -> B.ByteString
prefixToByteString (PrefixServer s) = s
prefixToByteString (PrefixNick n Nothing Nothing) = n
prefixToByteString (PrefixNick n (Just u) Nothing) = n `mappend` "!" `mappend` u
prefixToByteString (PrefixNick n Nothing (Just h)) = n `mappend` "@" `mappend` h
prefixToByteString (PrefixNick n (Just u) (Just h)) = mconcat [n,"!",u,"@",h]

paramsToByteString :: Params -> B.ByteString
paramsToByteString (Params [] Nothing) = mempty
paramsToByteString (Params m@(_:_) Nothing) = " " `mappend` B.unwords m
paramsToByteString (Params [] (Just t)) = " :" `mappend` t
paramsToByteString (Params m@(_:_) (Just t)) = mconcat [" ",B.unwords m," :",t]
