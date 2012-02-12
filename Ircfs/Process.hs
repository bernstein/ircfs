{-# LANGUAGE OverloadedStrings #-}
-- -----------------------------------------------------------------------------
-- |
-- Module      :  Ircfs.Process
-- Copyright   :  (c) Andreas-Christoph Bernstein 2011
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  andreas.bernstein@googlemail.com
-- Stability   :  unstable
-- Portability :  not portable
--
-- Process incoming file system messages.
--------------------------------------------------------------------------------
module Ircfs.Process
  ( 
    processIrc
  ) where

import           Prelude hiding ((.), id)
import           Control.Category
import qualified Data.Lens.Common as L
import qualified Data.ByteString.Char8 as B
import           Data.Monoid
import           Data.Maybe (maybeToList, fromMaybe)
import           Foreign.C.Types (CTime)

import qualified Network.IRC.Message as I
import           Ircfs.Types
import           Ircfs.Filesystem
import           Ircfs.Misc (Endomorphism, stamp')

-- | Process incomming irc messages.
processIrc :: CTime -> I.Message -> Endomorphism Fs
processIrc time (I.Message _ (I.CmdNumericReply 001) (I.Params _ _)) st =
  let now s = B.pack $ stamp' (timeZone s) time
      text s = mconcat [now s," RPL_WELCOME -- connected\n"]
  in  event time (text st) st
processIrc time m@(I.Message _ (I.CmdNumericReply 433) _) st =
  let now s = B.pack $ stamp' (timeZone s) time
      text s = mconcat [
                    now s," ERR_NICKNAMEINUSE "
                  , I.encode m
                  , "please change your nick, or you will be disconneted at any moment now.\n"
                  ]
  in  event time (text st) st
processIrc time (I.Message (Just (I.PrefixNick n _ _)) I.NICK (I.Params [] (Just new))) st =
  let yourNick = L.getL nickLens
      someone = event time (mconcat [n," changed nick to ",new,"\n"])
      you =  event time (mconcat ["You're now known as ",new,"\n"])
           . touch Qnick time
           . substitute Qnick new
  in  if Just n == yourNick st then you st else someone st
processIrc time (I.Message (Just (I.PrefixNick n _ _)) I.NICK (I.Params (new:_) _)) st =
  let yourNick s = fromMaybe mempty (L.getL nickLens s) -- nick st
      you =  event time (mconcat ["You're now known as ",new,"\n"])
           . touch Qnick time
           . substitute Qnick new
      someone = event time (mconcat [n," changed nick to ",new,"\n"])
  in  if n == yourNick st then you st else someone st
processIrc time (I.Message (Just (I.PrefixNick n _ _)) I.JOIN (I.Params [] (Just c))) st =
  let yourNick = L.getL nickLens
      ins = insertChannel c time
      -- someone = "add user to users file"
  in  if Just n == yourNick st then ins st else st
processIrc time (I.Message (Just (I.PrefixNick n _ _)) I.JOIN (I.Params (c:_) _)) st =
  let yourNick = L.getL nickLens
      ins = insertChannel c time
      -- someone = "add user to users file"
  in  if Just n == yourNick st then ins st else st
processIrc time (I.Message (Just (I.PrefixNick n _ _)) I.PART (I.Params (c:_) _)) st =
  let yourNick = L.getL nickLens
      rm = removeChannel c time
      -- someone = "say user left to users file"
  in  if Just n == yourNick st then rm st else st
processIrc time (I.Message Nothing I.PRIVMSG (I.Params (c:cs) t)) st =
  let n = fromMaybe mempty (L.getL nickLens st) -- nick st
      tm = L.getL (targetMapLens' c) st
      now s = B.pack $ stamp' (timeZone s) time
      ts = maybeToList t
      f k = touch (Qdata k) time
           . append (Qdata k) (mconcat [now st," < ",n,"> ",B.unwords (cs++ts),"\n"])
  in maybe st (`f` st) tm
processIrc time (I.Message (Just (I.PrefixNick n _ _)) I.PRIVMSG (I.Params (c:cs) t)) st =
  let tm = L.getL (targetMapLens' c) st
      ts = maybeToList t
      now s = B.pack $ stamp' (timeZone s) time
      f k s =  touch (Qdata k) time
        . append (Qdata k) (mconcat [now s," < ",n,"> ",B.unwords (cs++ts),"\n"])
        $ s
  in maybe st (`f` st) tm
processIrc time m@(I.Message _ I.ERROR _) st =
  event time (mconcat ["error ",I.encode m]) st
processIrc _ _ st = st

privmsg :: [B.ByteString] -> B.ByteString -> I.Message
privmsg targets x = I.Message Nothing I.PRIVMSG (I.Params targets (Just x))

