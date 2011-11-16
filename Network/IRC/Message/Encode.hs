{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module:   Network.IRC.Message.Encod
-- Copyright: (c) Andreas-Christoph Bernstein 2011
-- License: BSD3-style (see LICENSE)
-- Maintainer: Andreas-Christoph Bernstein <andreas.bernstein@googlemail.com>
-- Stability: experimental
-- Portability: portable
--
-- Serialize a Message value as a lazy 'L.ByteString'.

module Network.IRC.Message.Encode
  (
    fromMessage
  , fromPrefix
  , fromCommand
  , fromParams
  , encode
  ) where

import           Blaze.ByteString.Builder
import           Network.IRC.Message.Types
import           Data.Monoid
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as B hiding (map)

-- | Encode a Message to a 'Builder'.
fromMessage :: Message -> Builder
fromMessage (Message Nothing cmd ps) = 
  mconcat [ fromCommand cmd , fromParams ps , fromByteString "\r\n"]
fromMessage (Message (Just p) cmd ps) = 
  mconcat [ fromByteString ":" 
          , fromPrefix p
          , fromByteString " "
          , fromCommand cmd
          , fromParams ps
          , fromByteString "\r\n"]

-- | Encode a Command to a 'Builder'.
fromCommand :: Command -> Builder
fromCommand (CmdNumericReply x)
  | x < 10 = fromByteString "00" `mappend` fromByteString (B.pack (show x))
  | x < 100 = fromByteString "0" `mappend` fromByteString (B.pack (show x))
  | otherwise = fromByteString . B.pack . show $ x
fromCommand c = fromByteString . B.pack . show $ c

-- | Encode a Prefix to a 'Builder'.
fromPrefix :: Prefix -> Builder
fromPrefix (PrefixServer s) = fromByteString s
fromPrefix (PrefixNick n Nothing Nothing)   = fromByteString n
fromPrefix (PrefixNick n (Just u) Nothing)  = mconcat [ fromByteString n
                                                      , fromByteString "!"
                                                      , fromByteString u]
fromPrefix (PrefixNick n Nothing (Just h))  = mconcat [ fromByteString n
                                                      , fromByteString"@"
                                                      , fromByteString h]
fromPrefix (PrefixNick n (Just u) (Just h)) = mconcat [ fromByteString n
                                                      , fromByteString "!"
                                                      , fromByteString u
                                                      , fromByteString "@"
                                                      , fromByteString h]

-- | Encode Params to a 'Builder'.
fromParams :: Params -> Builder
fromParams (Params [] Nothing) = mempty
fromParams (Params m@(_:_) Nothing) = 
  fromByteString " " `mappend` fromByteString (B.unwords m)
fromParams (Params [] (Just t)) = 
  fromByteString " :" `mappend` fromByteString t
fromParams (Params m@(_:_) (Just t)) = 
  mconcat [ fromByteString " ",fromByteString (B.unwords m),
            fromByteString " :",fromByteString t]

-- | Serialize a Message as a lazy 'L.ByteString'
--encode :: Message -> L.ByteString
--encode = toLazyByteString . fromMessage
encode :: Message -> B.ByteString
encode = toByteString . fromMessage
