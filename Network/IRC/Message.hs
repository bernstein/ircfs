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
  , channel
  , nick
  , Command(..)
  , Prefix(..)
  , Params(..)
  , encode
  ) where

import Network.IRC.Message.Types
import Network.IRC.Message.Parser
import Network.IRC.Message.Encode

