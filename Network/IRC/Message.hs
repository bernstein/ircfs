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

